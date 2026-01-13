package config

import (
	"errors"
	"os"
	"path/filepath"
	"strings"

	"github.com/pelletier/go-toml/v2"
)

type Config struct {
	// CredentialsPath is the path to Google OAuth client credentials JSON.
	CredentialsPath string `toml:"credentials_path"`

	// TokenPath is the path where the OAuth token is stored.
	TokenPath string `toml:"token_path"`

	// Debug enables verbose logging in the Go tool.
	Debug bool `toml:"debug"`
}

func DefaultConfig() *Config {
	return &Config{
		CredentialsPath: filepath.Join(GetConfigDir(), "credentials.json"),
		TokenPath:       filepath.Join(GetDataDir(), "token.json"),
		Debug:           false,
	}
}

func ConfigPath() string {
	return filepath.Join(GetConfigDir(), "config.toml")
}

func LoadConfig() (*Config, error) {
	configPath := ConfigPath()

	if _, err := os.Stat(configPath); errors.Is(err, os.ErrNotExist) {
		cfg := DefaultConfig()
		if err := SaveConfig(cfg); err != nil {
			return cfg, err
		}
		return cfg, nil
	}

	data, err := os.ReadFile(configPath)
	if err != nil {
		return nil, err
	}

	var cfg Config
	if err := toml.Unmarshal(data, &cfg); err != nil {
		return nil, err
	}

	cfg.CredentialsPath, err = ExpandPath(cfg.CredentialsPath)
	if err != nil {
		return nil, err
	}

	cfg.TokenPath, err = ExpandPath(cfg.TokenPath)
	if err != nil {
		return nil, err
	}

	return &cfg, nil
}

func SaveConfig(cfg *Config) error {
	if err := os.MkdirAll(GetConfigDir(), 0o755); err != nil {
		return err
	}

	data, err := toml.Marshal(cfg)
	if err != nil {
		return err
	}

	return os.WriteFile(ConfigPath(), data, 0o644)
}

func ExpandPath(path string) (string, error) {
	if path == "" {
		return "", nil
	}

	if path == "~" || strings.HasPrefix(path, "~/") {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", err
		}
		if path == "~" {
			return home, nil
		}
		return filepath.Join(home, path[2:]), nil
	}

	return path, nil
}
