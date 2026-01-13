package gdocs

import (
	"context"
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"time"

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
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		return nil, fmt.Errorf("unable to start localhost callback listener: %w", err)
	}
	defer l.Close()

	port := l.Addr().(*net.TCPAddr).Port
	redirectURL := fmt.Sprintf("http://localhost:%d/oauth2callback", port)
	cfg.RedirectURL = redirectURL

	state, err := randomState()
	if err != nil {
		return nil, err
	}

	authURL := cfg.AuthCodeURL(state, oauth2.AccessTypeOffline)
	fmt.Fprintln(os.Stderr, "Authorize this application in your browser.")
	fmt.Fprintln(os.Stderr, authURL)
	fmt.Fprintf(os.Stderr, "Waiting for OAuth redirect on %s ...\n", redirectURL)

	codeCh := make(chan string, 1)
	errCh := make(chan error, 1)

	mux := http.NewServeMux()
	srv := &http.Server{
		Handler:           mux,
		ReadHeaderTimeout: 5 * time.Second,
	}

	mux.HandleFunc("/oauth2callback", func(w http.ResponseWriter, r *http.Request) {
		if got := r.URL.Query().Get("state"); got != state {
			http.Error(w, "Invalid OAuth state; please retry.", http.StatusBadRequest)
			return
		}

		if e := r.URL.Query().Get("error"); e != "" {
			http.Error(w, "OAuth error: "+e, http.StatusBadRequest)
			select {
			case errCh <- fmt.Errorf("oauth error: %s", e):
			default:
			}
			return
		}

		code := r.URL.Query().Get("code")
		if code == "" {
			http.Error(w, "Missing OAuth code; please retry.", http.StatusBadRequest)
			return
		}

		fmt.Fprintln(w, "Authorization complete. You may close this window.")

		select {
		case codeCh <- code:
		default:
		}
	})

	go func() {
		if err := srv.Serve(l); err != nil && !errors.Is(err, http.ErrServerClosed) {
			select {
			case errCh <- err:
			default:
			}
		}
	}()

	// macOS convenience: attempt to open the URL in the user's browser.
	_ = exec.Command("open", authURL).Start()

	var code string
	select {
	case code = <-codeCh:
	case err := <-errCh:
		_ = srv.Close()
		return nil, err
	case <-time.After(10 * time.Minute):
		_ = srv.Close()
		return nil, fmt.Errorf("oauth flow timed out waiting for browser redirect")
	}

	shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	_ = srv.Shutdown(shutdownCtx)

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

func randomState() (string, error) {
	var b [32]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("unable to generate oauth state: %w", err)
	}
	return base64.RawURLEncoding.EncodeToString(b[:]), nil
}
