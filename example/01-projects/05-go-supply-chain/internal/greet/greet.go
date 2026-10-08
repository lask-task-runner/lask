// Package greet builds the greeting hello prints.
package greet

import "fmt"

// Greeting returns the line hello prints for name.
func Greeting(name, version string) string {
	if name == "" {
		name = "World"
	}
	return fmt.Sprintf("Hello, %s! (hello %s)", name, version)
}
