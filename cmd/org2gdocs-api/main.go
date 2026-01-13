package main

import (
	"flag"
	"fmt"
	"os"

	"org2gdocs/config"
	"org2gdocs/debug"
)

func main() {
	flags := flag.NewFlagSet("org2gdocs-api", flag.ContinueOnError)
	flags.SetOutput(os.Stderr)

	debugFlag := flags.Bool("debug", false, "Enable debug logging (stderr)")
	if err := flags.Parse(os.Args[1:]); err != nil {
		os.Exit(2)
	}

	args := flags.Args()
	if len(args) > 0 && args[0] != "auth" {
		usage()
		os.Exit(2)
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		if len(args) > 0 && args[0] == "auth" {
			fmt.Fprintf(os.Stderr, "Failed to load config: %v\n", err)
			os.Exit(1)
		}
		_ = writeErrorResult(os.Stdout, errCodeConfigError, "failed to load config", err)
		os.Exit(1)
	}

	debug.Enabled = *debugFlag || cfg.Debug

	if len(args) > 0 && args[0] == "auth" {
		if err := handleAuth(cfg); err != nil {
			fmt.Fprintf(os.Stderr, "Auth failed: %v\n", err)
			os.Exit(1)
		}
		return
	}

	handleOperation()
}

func usage() {
	fmt.Fprintln(os.Stderr, "Usage:")
	fmt.Fprintln(os.Stderr, "  org2gdocs-api [--debug] auth    # perform OAuth flow")
	fmt.Fprintln(os.Stderr, "  org2gdocs-api [--debug]         # read (operation ...) from stdin")
	fmt.Fprintln(os.Stderr)
	fmt.Fprintln(os.Stderr, "Config:")
	fmt.Fprintf(os.Stderr, "  %s\n", config.ConfigPath())
}
