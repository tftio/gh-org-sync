package main

import (
	"context"
	"fmt"
	"os"

	"org2gdocs/config"
	"org2gdocs/debug"
	"org2gdocs/gdocs"
)

func handleAuth(cfg *config.Config) error {
	_, err := gdocs.NewClient(context.Background(), cfg)
	if err != nil {
		return err
	}

	debug.Log("Auth sanity check: API services created successfully")

	fmt.Fprintln(os.Stderr, "Authentication complete.")
	fmt.Fprintf(os.Stderr, "Token stored at: %s\n", cfg.TokenPath)
	return nil
}
