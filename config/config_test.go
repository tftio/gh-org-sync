package config

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadConfig_CreatesDefaultWhenMissing(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(tmp, "cfg"))
	t.Setenv("XDG_DATA_HOME", filepath.Join(tmp, "data"))

	cfg, err := LoadConfig()
	if err != nil {
		t.Fatalf("LoadConfig() error: %v", err)
	}

	wantCredentials := filepath.Join(GetConfigDir(), "credentials.json")
	wantToken := filepath.Join(GetDataDir(), "token.json")

	if cfg.CredentialsPath != wantCredentials {
		t.Fatalf("CredentialsPath=%q, want %q", cfg.CredentialsPath, wantCredentials)
	}
	if cfg.TokenPath != wantToken {
		t.Fatalf("TokenPath=%q, want %q", cfg.TokenPath, wantToken)
	}
	if cfg.Debug {
		t.Fatalf("Debug=true, want false")
	}

	if _, err := os.Stat(ConfigPath()); err != nil {
		t.Fatalf("expected config file at %s: %v", ConfigPath(), err)
	}
}

func TestExpandPath_Tilde(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("HOME", tmp)

	got, err := ExpandPath("~/a/b")
	if err != nil {
		t.Fatalf("ExpandPath() error: %v", err)
	}

	want := filepath.Join(tmp, "a", "b")
	if got != want {
		t.Fatalf("ExpandPath()=%q, want %q", got, want)
	}
}
