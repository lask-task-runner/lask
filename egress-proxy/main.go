// lask-egress-proxy forwards the connections of a container given
// `egress` to the destinations it lists, and refuses every other (lask
// spec 10.12). lask starts it with the entries as its arguments, and
// reads what it refuses from its standard output, one JSON object a
// line.
package main

import (
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"time"
)

func main() {
	listen := flag.String("listen", ":3128", "address to listen on")
	flag.Parse()
	var list List
	for _, arg := range flag.Args() {
		e, err := ParseEntry(arg)
		if err != nil {
			fmt.Fprintln(os.Stderr, "lask-egress-proxy:", err)
			os.Exit(2)
		}
		list = append(list, e)
	}
	ln, err := net.Listen("tcp", *listen)
	if err != nil {
		fmt.Fprintln(os.Stderr, "lask-egress-proxy:", err)
		os.Exit(1)
	}
	p := newProxy(list)
	p.emit(event{Event: "ready"})
	srv := &http.Server{Handler: p, ReadHeaderTimeout: 30 * time.Second}
	if err := srv.Serve(ln); err != nil {
		fmt.Fprintln(os.Stderr, "lask-egress-proxy:", err)
		os.Exit(1)
	}
}
