// Command hello prints a greeting and the version it was built as.
package main

import (
	"flag"
	"fmt"

	"example.com/hello/internal/greet"
)

// Set at build time: -ldflags "-X main.version=...".
var version = "dev"

func main() {
	name := flag.String("name", "", "who to greet")
	flag.Parse()
	fmt.Println(greet.Greeting(*name, version))
}
