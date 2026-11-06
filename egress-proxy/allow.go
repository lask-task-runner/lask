// The entries of `egress` and what they admit (lask spec 10.12, Entries).
package main

import (
	"fmt"
	"net/netip"
	"strconv"
	"strings"
)

// An Entry is one `<host>:<port>` of `egress`: a name, a wildcard
// (`*.example.com`, kept here as `example.com` with Wildcard set), or
// an address.
type Entry struct {
	Name     string
	Wildcard bool
	Addr     netip.Addr
	Port     uint16
}

// ParseEntry reads an entry as lask writes it.
func ParseEntry(s string) (Entry, error) {
	i := strings.LastIndexByte(s, ':')
	if i <= 0 {
		return Entry{}, fmt.Errorf("%q is not <host>:<port>", s)
	}
	host, portText := s[:i], s[i+1:]
	port, err := strconv.ParseUint(portText, 10, 16)
	if err != nil || port == 0 || portText != strconv.FormatUint(port, 10) {
		return Entry{}, fmt.Errorf("%q has no port from 1 to 65535", s)
	}
	e := Entry{Port: uint16(port)}
	if strings.HasPrefix(host, "[") && strings.HasSuffix(host, "]") {
		a, err := netip.ParseAddr(host[1 : len(host)-1])
		if err != nil || !a.Is6() || a.Zone() != "" {
			return Entry{}, fmt.Errorf("%q is not an IPv6 address", host)
		}
		e.Addr = a
		return e, nil
	}
	if a, err := netip.ParseAddr(host); err == nil {
		if !a.Is4() {
			return Entry{}, fmt.Errorf("%q: an IPv6 address is written in brackets", s)
		}
		e.Addr = a
		return e, nil
	}
	name := normalizeName(host)
	if rest, ok := strings.CutPrefix(name, "*."); ok {
		e.Wildcard = true
		name = rest
	}
	if !validName(name) {
		return Entry{}, fmt.Errorf("%q is not a domain name", host)
	}
	e.Name = name
	return e, nil
}

func normalizeName(n string) string {
	return strings.TrimSuffix(strings.ToLower(n), ".")
}

// validName accepts LDH labels (letters, digits, hyphens, and the
// underscore some service names carry), which is how an
// internationalized name is written (`xn--...`).
func validName(n string) bool {
	if n == "" || len(n) > 253 {
		return false
	}
	for _, label := range strings.Split(n, ".") {
		if label == "" || len(label) > 63 || label[0] == '-' || label[len(label)-1] == '-' {
			return false
		}
		for _, c := range label {
			if !(c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '-' || c == '_') {
				return false
			}
		}
	}
	return true
}

// Admits says whether the entry admits a destination: its port, and a
// host that is the name it gives or one under its wildcard, or the
// address it gives.
func (e Entry) Admits(host string, port uint16) bool {
	if port != e.Port {
		return false
	}
	if a, err := netip.ParseAddr(strings.Trim(host, "[]")); err == nil {
		return e.Addr.IsValid() && a.Unmap() == e.Addr.Unmap()
	}
	if e.Addr.IsValid() {
		return false
	}
	h := normalizeName(host)
	if e.Wildcard {
		return strings.HasSuffix(h, "."+e.Name)
	}
	return h == e.Name
}

// List is what `egress` admits.
type List []Entry

func (l List) Admits(host string, port uint16) bool {
	for _, e := range l {
		if e.Admits(host, port) {
			return true
		}
	}
	return false
}

// names reports whether an entry names the address itself, which is
// the only way an address refused below is reached.
func (l List) names(a netip.Addr, port uint16) bool {
	for _, e := range l {
		if e.Addr.IsValid() && e.Addr.Unmap() == a.Unmap() && e.Port == port {
			return true
		}
	}
	return false
}

// Refused says why an address a destination resolved to is not
// connected to, or "" when it may be: loopback, link-local (the cloud
// metadata address among them), multicast and unspecified addresses
// are refused unless an entry names the address.
func (l List) Refused(a netip.Addr, port uint16) string {
	a = a.Unmap()
	var kind string
	switch {
	case a.IsLoopback():
		kind = "loopback"
	case a.IsLinkLocalUnicast(), a.IsLinkLocalMulticast():
		kind = "link-local"
	case a.IsMulticast(), a.IsInterfaceLocalMulticast():
		kind = "multicast"
	case a.IsUnspecified():
		kind = "unspecified"
	default:
		return ""
	}
	if l.names(a, port) {
		return ""
	}
	return kind + " address " + a.String()
}
