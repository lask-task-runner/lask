package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httputil"
	"net/netip"
	"os"
	"strconv"
	"sync"
	"time"
)

// Resolver is how a name is resolved: by the proxy itself, so that
// what is checked is what is connected to.
type Resolver interface {
	LookupNetIP(ctx context.Context, network, host string) ([]netip.Addr, error)
}

// Proxy forwards what its list admits (lask spec 10.12).
type Proxy struct {
	List     List
	Resolver Resolver
	Dialer   *net.Dialer
	Log      io.Writer

	logMu sync.Mutex
}

// event is one line of the log lask reads: a destination refused, or
// the proxy being ready.
type event struct {
	Event       string `json:"event"`
	Destination string `json:"destination,omitempty"`
	Reason      string `json:"reason,omitempty"`
}

func (p *Proxy) emit(e event) {
	p.logMu.Lock()
	defer p.logMu.Unlock()
	b, _ := json.Marshal(e)
	fmt.Fprintf(p.Log, "%s\n", b)
}

// errRefused is a destination the proxy will not reach, with why.
type errRefused struct{ reason string }

func (e errRefused) Error() string { return e.reason }

// dial connects to a destination the list admits, at an address that
// is not refused. A name is resolved here and the address dialled is
// one it resolved to, so that it answers once per connection.
func (p *Proxy) dial(ctx context.Context, host string, port uint16) (net.Conn, error) {
	if !p.List.Admits(host, port) {
		return nil, errRefused{"not in egress"}
	}
	var addrs []netip.Addr
	if a, err := netip.ParseAddr(host); err == nil {
		addrs = []netip.Addr{a}
	} else {
		found, err := p.Resolver.LookupNetIP(ctx, "ip", host)
		if err != nil {
			return nil, fmt.Errorf("resolving %s: %w", host, err)
		}
		addrs = found
	}
	var refusal string
	var lastErr error
	for _, a := range addrs {
		if why := p.List.Refused(a, port); why != "" {
			refusal = why
			continue
		}
		c, err := p.Dialer.DialContext(ctx, "tcp", netip.AddrPortFrom(a.Unmap(), port).String())
		if err == nil {
			return c, nil
		}
		lastErr = err
	}
	if lastErr != nil {
		return nil, lastErr
	}
	if refusal != "" {
		return nil, errRefused{refusal}
	}
	return nil, fmt.Errorf("%s resolved to no address", host)
}

// destination splits `host:port`, taking defaultPort when there is none.
func destination(hostport string, defaultPort uint16) (string, uint16, error) {
	host, portText, err := net.SplitHostPort(hostport)
	if err != nil {
		return hostport, defaultPort, nil
	}
	n, err := strconv.ParseUint(portText, 10, 16)
	if err != nil || n == 0 {
		return "", 0, fmt.Errorf("bad port in %q", hostport)
	}
	return host, uint16(n), nil
}

func (p *Proxy) refuse(w http.ResponseWriter, host string, port uint16, err error) {
	dest := net.JoinHostPort(host, strconv.Itoa(int(port)))
	var r errRefused
	if errors.As(err, &r) {
		p.emit(event{Event: "denied", Destination: dest, Reason: r.reason})
		http.Error(w, fmt.Sprintf("lask egress: %s is refused: %s", dest, r.reason), http.StatusForbidden)
		return
	}
	http.Error(w, fmt.Sprintf("lask egress: %s cannot be reached: %v", dest, err), http.StatusBadGateway)
}

func (p *Proxy) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodConnect {
		p.connect(w, r)
		return
	}
	if r.URL.Scheme != "http" || r.URL.Host == "" {
		http.Error(w, "lask egress: a request to the proxy names an http:// destination, or uses CONNECT", http.StatusBadRequest)
		return
	}
	host, port, err := destination(r.URL.Host, 80)
	if err != nil {
		http.Error(w, "lask egress: "+err.Error(), http.StatusBadRequest)
		return
	}
	conn, err := p.dial(r.Context(), host, port)
	if err != nil {
		p.refuse(w, host, port, err)
		return
	}
	// The connection is the one checked; the transport is given it and
	// no other.
	used := false
	transport := &http.Transport{
		DialContext: func(context.Context, string, string) (net.Conn, error) {
			if used {
				return nil, errors.New("one connection per request")
			}
			used = true
			return conn, nil
		},
		DisableKeepAlives: true,
	}
	rp := &httputil.ReverseProxy{
		Rewrite:   func(pr *httputil.ProxyRequest) { pr.Out.URL = r.URL },
		Transport: transport,
	}
	rp.ServeHTTP(w, r)
}

func (p *Proxy) connect(w http.ResponseWriter, r *http.Request) {
	host, port, err := destination(r.Host, 0)
	if err != nil || port == 0 {
		http.Error(w, "lask egress: CONNECT names <host>:<port>", http.StatusBadRequest)
		return
	}
	upstream, err := p.dial(r.Context(), host, port)
	if err != nil {
		p.refuse(w, host, port, err)
		return
	}
	hj, ok := w.(http.Hijacker)
	if !ok {
		upstream.Close()
		http.Error(w, "lask egress: cannot tunnel", http.StatusInternalServerError)
		return
	}
	client, buf, err := hj.Hijack()
	if err != nil {
		upstream.Close()
		return
	}
	if _, err := client.Write([]byte("HTTP/1.1 200 Connection Established\r\n\r\n")); err != nil {
		client.Close()
		upstream.Close()
		return
	}
	// What the client sent after the request is already buffered.
	if n := buf.Reader.Buffered(); n > 0 {
		pending, _ := buf.Reader.Peek(n)
		if _, err := upstream.Write(pending); err != nil {
			client.Close()
			upstream.Close()
			return
		}
	}
	go pipe(upstream, client)
	pipe(client, upstream)
}

// pipe copies one direction of a tunnel, and closes both ends when it
// is done, so that the other direction ends too.
func pipe(dst, src net.Conn) {
	io.Copy(dst, src)
	dst.Close()
	src.Close()
}

func newProxy(list List) *Proxy {
	return &Proxy{
		List:     list,
		Resolver: net.DefaultResolver,
		Dialer:   &net.Dialer{Timeout: 30 * time.Second},
		Log:      os.Stdout,
	}
}
