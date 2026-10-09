package greet

import "testing"

func TestGreeting(t *testing.T) {
	cases := []struct{ name, want string }{
		{"Lask", "Hello, Lask! (hello v1)"},
		{"", "Hello, World! (hello v1)"},
	}
	for _, c := range cases {
		if got := Greeting(c.name, "v1"); got != c.want {
			t.Errorf("Greeting(%q) = %q, want %q", c.name, got, c.want)
		}
	}
}
