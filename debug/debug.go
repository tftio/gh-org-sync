package debug

import (
	"fmt"
	"io"
	"os"
	"sync"
	"time"
)

// Enabled controls whether debug logging is emitted.
//
// When enabled, logs are written to stderr by default.
var Enabled bool

var (
	mu     sync.Mutex
	writer io.Writer = os.Stderr
)

// SetWriter overrides the destination for debug logs.
func SetWriter(w io.Writer) {
	mu.Lock()
	defer mu.Unlock()
	writer = w
}

// Log writes a formatted debug line when debug is enabled.
func Log(format string, args ...any) {
	if !Enabled {
		return
	}

	mu.Lock()
	defer mu.Unlock()

	timestamp := time.Now().Format("15:04:05.000")
	fmt.Fprintf(writer, "[DEBUG %s] ", timestamp)
	fmt.Fprintf(writer, format, args...)
	fmt.Fprintln(writer)
}

// LogSexp writes a multi-line S-expression debug payload when debug is enabled.
func LogSexp(label string, sexp string) {
	if !Enabled {
		return
	}

	mu.Lock()
	defer mu.Unlock()

	timestamp := time.Now().Format("15:04:05.000")
	fmt.Fprintf(writer, "[DEBUG SEXP %s %s]\n%s\n", label, timestamp, sexp)
}
