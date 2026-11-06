package main

import (
	"net/netip"
	"testing"
)

func mustList(t *testing.T, entries ...string) List {
	t.Helper()
	var l List
	for _, s := range entries {
		e, err := ParseEntry(s)
		if err != nil {
			t.Fatalf("ParseEntry(%q): %v", s, err)
		}
		l = append(l, e)
	}
	return l
}

func TestParseEntry(t *testing.T) {
	for _, s := range []string{"registry.npmjs.org:443", "*.github.com:443", "Example.COM.:80", "10.0.0.1:5432", "[2001:db8::1]:443", "xn--bcher-kva.example:443", "_svc.example.com:443"} {
		if _, err := ParseEntry(s); err != nil {
			t.Errorf("ParseEntry(%q): %v", s, err)
		}
	}
	for _, s := range []string{"example.com", "example.com:0", "example.com:65536", "example.com:08", "*:443", "*.*.com:443", "a*.com:443", "2001:db8::1:443", "[10.0.0.1]:443", "http://example.com:80", "example.com/x:80", ":443", "-a.com:443", "10.0.0.0/8:443", "example.com:1-2"} {
		if _, err := ParseEntry(s); err == nil {
			t.Errorf("ParseEntry(%q) accepted it", s)
		}
	}
}

func TestAdmits(t *testing.T) {
	l := mustList(t, "registry.npmjs.org:443", "*.github.com:443", "10.0.0.1:5432", "[2001:db8::1]:443")
	cases := []struct {
		host string
		port uint16
		want bool
	}{
		{"registry.npmjs.org", 443, true},
		{"REGISTRY.npmjs.org.", 443, true},
		{"registry.npmjs.org", 80, false},
		{"evil-registry.npmjs.org", 443, false},
		{"api.github.com", 443, true},
		{"a.b.github.com", 443, true},
		{"github.com", 443, false},
		{"evilgithub.com", 443, false},
		{"10.0.0.1", 5432, true},
		{"10.0.0.2", 5432, false},
		{"::ffff:10.0.0.1", 5432, true},
		{"[2001:db8::1]", 443, true},
		{"2001:db8::1", 443, true},
	}
	for _, c := range cases {
		if got := l.Admits(c.host, c.port); got != c.want {
			t.Errorf("Admits(%q, %d) = %v, want %v", c.host, c.port, got, c.want)
		}
	}
}

func TestRefused(t *testing.T) {
	l := mustList(t, "127.0.0.1:8080")
	for _, s := range []string{"127.0.0.1", "::1", "169.254.169.254", "fe80::1", "224.0.0.1", "0.0.0.0", "::", "::ffff:127.0.0.2"} {
		a := netip.MustParseAddr(s)
		port := uint16(80)
		if l.Refused(a, port) == "" {
			t.Errorf("Refused(%s) allowed it", s)
		}
	}
	for _, s := range []string{"10.0.0.1", "192.168.1.1", "172.17.0.2", "93.184.216.34", "2001:db8::1"} {
		if why := l.Refused(netip.MustParseAddr(s), 80); why != "" {
			t.Errorf("Refused(%s) = %q", s, why)
		}
	}
	// An entry naming the address is how a refused address is reached.
	if why := l.Refused(netip.MustParseAddr("127.0.0.1"), 8080); why != "" {
		t.Errorf("an address the list names was refused: %s", why)
	}
}
