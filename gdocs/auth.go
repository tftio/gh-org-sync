package gdocs

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"org2gdocs/debug"

	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"google.golang.org/api/docs/v1"
	"google.golang.org/api/drive/v3"
)

func SetupOAuth(credentialsPath, tokenPath string) (*http.Client, error) {
	debug.Log("Reading credentials from: %s", credentialsPath)

	b, err := os.ReadFile(credentialsPath)
	if err != nil {
		return nil, fmt.Errorf("unable to read credentials: %w", err)
	}

	oauthCfg, err := google.ConfigFromJSON(b, docs.DocumentsScope, drive.DriveFileScope)
	if err != nil {
		return nil, fmt.Errorf("unable to parse credentials: %w", err)
	}

	debug.Log("Checking for existing token at: %s", tokenPath)
	tok, err := tokenFromFile(tokenPath)
	if err != nil {
		return nil, err
	}

	if tok == nil {
		debug.Log("No token found, starting OAuth flow")
		tok, err = tokenFromWeb(oauthCfg)
		if err != nil {
			return nil, err
		}
		if err := saveToken(tokenPath, tok); err != nil {
			return nil, err
		}
	} else {
		debug.Log("Using existing token")
	}

	return oauthCfg.Client(context.Background(), tok), nil
}

func tokenFromFile(path string) (*oauth2.Token, error) {
	f, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("unable to read token file: %w", err)
	}
	defer f.Close()

	var tok oauth2.Token
	if err := json.NewDecoder(f).Decode(&tok); err != nil {
		return nil, fmt.Errorf("unable to decode token file: %w", err)
	}
	return &tok, nil
}

func tokenFromWeb(cfg *oauth2.Config) (*oauth2.Token, error) {
	authURL := cfg.AuthCodeURL("state-token", oauth2.AccessTypeOffline)
	fmt.Fprintln(os.Stderr, "Authorize this application by visiting the URL below:")
	fmt.Fprintln(os.Stderr, authURL)
	fmt.Fprintln(os.Stderr)
	fmt.Fprintln(os.Stderr, "After approving, paste the authorization code and press Enter:")

	// macOS convenience: attempt to open the URL in the user's browser.
	_ = exec.Command("open", authURL).Start()

	reader := bufio.NewReader(os.Stdin)
	code, err := reader.ReadString('\n')
	if err != nil {
		return nil, fmt.Errorf("unable to read authorization code: %w", err)
	}
	code = strings.TrimSpace(code)

	tok, err := cfg.Exchange(context.Background(), code)
	if err != nil {
		return nil, fmt.Errorf("unable to retrieve token from web: %w", err)
	}
	return tok, nil
}

func saveToken(path string, tok *oauth2.Token) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}

	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0o600)
	if err != nil {
		return fmt.Errorf("unable to cache oauth token: %w", err)
	}
	defer f.Close()

	if err := json.NewEncoder(f).Encode(tok); err != nil {
		return fmt.Errorf("unable to write oauth token: %w", err)
	}

	debug.Log("Saved OAuth token to: %s", path)
	return nil
}
