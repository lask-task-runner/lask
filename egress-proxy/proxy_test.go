package main

import (
	"bufio"
	"bytes"
	"context"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"net/url"
	"strings"
	"testing"
	"time"
)

// fakeResolver answers each name with the addresses it is given.
type fakeResolver map[string][]netip.Addr

func (f fakeResolver) LookupNetIP(_ context.Context, _, host string) ([]netip.Addr, error) {
	if a, ok := f[host]; ok {
		return a, nil
	}
	return nil, fmt.Errorf("no such host %s", host)
}

// upstream is an origin server on loopback, which a list reaches only
// by naming its address as well.
func upstream(t *testing.T) (*httptest.Server, uint16) {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "hello from %s", r.Host)
	}))
	t.Cleanup(srv.Close)
	u, _ := url.Parse(srv.URL)
	_, port, _ := destination(u.Host, 0)
	return srv, port
}

func startProxy(t *testing.T, p *Proxy) string {
	t.Helper()
	srv := httptest.NewServer(p)
	t.Cleanup(srv.Close)
	return srv.URL
}

func client(t *testing.T, proxy string) *http.Client {
	u, _ := url.Parse(proxy)
	return &http.Client{Transport: &http.Transport{Proxy: http.ProxyURL(u)}, Timeout: 5 * time.Second}
}

func TestPlainHTTP(t *testing.T) {
	_, port := upstream(t)
	var log bytes.Buffer
	p := newProxy(mustList(t, fmt.Sprintf("origin.test:%d", port), fmt.Sprintf("127.0.0.1:%d", port)))
	p.Resolver = fakeResolver{"origin.test": {netip.MustParseAddr("127.0.0.1")}}
	p.Log = &log
	c := client(t, startProxy(t, p))

	res, err := c.Get(fmt.Sprintf("http://origin.test:%d/", port))
	if err != nil {
		t.Fatal(err)
	}
	body, _ := io.ReadAll(res.Body)
	res.Body.Close()
	if res.StatusCode != 200 || !strings.HasPrefix(string(body), "hello from origin.test") {
		t.Fatalf("admitted request: %d %q", res.StatusCode, body)
	}

	res, err = c.Get(fmt.Sprintf("http://other.test:%d/", port))
	if err != nil {
		t.Fatal(err)
	}
	body, _ = io.ReadAll(res.Body)
	res.Body.Close()
	if res.StatusCode != http.StatusForbidden || !strings.Contains(string(body), "other.test") {
		t.Fatalf("refused request: %d %q", res.StatusCode, body)
	}
	if !strings.Contains(log.String(), `{"event":"denied","destination":"other.test:`) {
		t.Fatalf("the refusal was not logged: %s", log.String())
	}
}

func TestConnect(t *testing.T) {
	srv, port := upstream(t)
	_ = srv
	p := newProxy(mustList(t, fmt.Sprintf("127.0.0.1:%d", port)))
	p.Log = io.Discard
	proxy := startProxy(t, p)
	u, _ := url.Parse(proxy)

	tunnel := func(target string) (int, net.Conn) {
		conn, err := net.Dial("tcp", u.Host)
		if err != nil {
			t.Fatal(err)
		}
		fmt.Fprintf(conn, "CONNECT %s HTTP/1.1\r\nHost: %s\r\n\r\n", target, target)
		res, err := http.ReadResponse(bufio.NewReader(conn), nil)
		if err != nil {
			t.Fatal(err)
		}
		return res.StatusCode, conn
	}

	code, conn := tunnel(fmt.Sprintf("127.0.0.1:%d", port))
	if code != 200 {
		t.Fatalf("admitted CONNECT: %d", code)
	}
	fmt.Fprintf(conn, "GET / HTTP/1.1\r\nHost: tunnelled\r\nConnection: close\r\n\r\n")
	res, err := http.ReadResponse(bufio.NewReader(conn), nil)
	if err != nil {
		t.Fatal(err)
	}
	body, _ := io.ReadAll(res.Body)
	conn.Close()
	if !strings.HasPrefix(string(body), "hello from tunnelled") {
		t.Fatalf("through the tunnel: %q", body)
	}

	if code, conn := tunnel(fmt.Sprintf("127.0.0.1:%d", port+1)); code != http.StatusForbidden {
		t.Fatalf("CONNECT to another port: %d", code)
	} else {
		conn.Close()
	}
}

// A listed name that resolves to a refused address is not reached: the
// cloud metadata address, or loopback, through a name.
func TestRefusedResolution(t *testing.T) {
	var log bytes.Buffer
	p := newProxy(mustList(t, "metadata.test:80", "local.test:80"))
	p.Resolver = fakeResolver{
		"metadata.test": {netip.MustParseAddr("169.254.169.254")},
		"local.test":    {netip.MustParseAddr("127.0.0.1")},
	}
	p.Log = &log
	c := client(t, startProxy(t, p))
	for _, host := range []string{"metadata.test", "local.test"} {
		res, err := c.Get("http://" + host + "/")
		if err != nil {
			t.Fatal(err)
		}
		res.Body.Close()
		if res.StatusCode != http.StatusForbidden {
			t.Errorf("%s: %d", host, res.StatusCode)
		}
	}
	if !strings.Contains(log.String(), "link-local address 169.254.169.254") || !strings.Contains(log.String(), "loopback address 127.0.0.1") {
		t.Fatalf("log: %s", log.String())
	}
}

func TestNotAProxyRequest(t *testing.T) {
	p := newProxy(nil)
	p.Log = io.Discard
	proxy := startProxy(t, p)
	res, err := http.Get(proxy + "/")
	if err != nil {
		t.Fatal(err)
	}
	res.Body.Close()
	if res.StatusCode != http.StatusBadRequest {
		t.Fatalf("an origin-form request: %d", res.StatusCode)
	}
}
