package config

import (
	"os"
	"path/filepath"
)

const appDirName = "org2gdocs"

func GetConfigDir() string {
	if xdgConfig := os.Getenv("XDG_CONFIG_HOME"); xdgConfig != "" {
		return filepath.Join(xdgConfig, appDirName)
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".config", appDirName)
}

func GetDataDir() string {
	if xdgData := os.Getenv("XDG_DATA_HOME"); xdgData != "" {
		return filepath.Join(xdgData, appDirName)
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".local", "share", appDirName)
}

func GetCacheDir() string {
	if xdgCache := os.Getenv("XDG_CACHE_HOME"); xdgCache != "" {
		return filepath.Join(xdgCache, appDirName)
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".cache", appDirName)
}
