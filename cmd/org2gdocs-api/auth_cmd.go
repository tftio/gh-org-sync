package main

import (
	"context"
	"fmt"
	"os"

	"org2gdocs/config"
	"org2gdocs/debug"

	"google.golang.org/api/docs/v1"
	"google.golang.org/api/option"
)

func handleAuth(cfg *config.Config) error {
	client, err := SetupOAuth(cfg.CredentialsPath, cfg.TokenPath)
	if err != nil {
		return err
	}

	// Best-effort sanity check: initialize the Docs service with the token.
	_, err = docs.NewService(
		context.Background(),
		option.WithHTTPClient(client),
		option.WithUserAgent("org2gdocs-api/0.1.0"),
	)
	if err != nil {
		return err
	}

	debug.Log("Auth sanity check: docs service created successfully")

	fmt.Fprintln(os.Stderr, "Authentication complete.")
	fmt.Fprintf(os.Stderr, "Token stored at: %s\n", cfg.TokenPath)
	return nil
}
