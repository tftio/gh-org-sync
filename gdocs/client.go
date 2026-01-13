package gdocs

import (
	"context"

	"org2gdocs/config"

	"google.golang.org/api/docs/v1"
	"google.golang.org/api/drive/v3"
	"google.golang.org/api/option"
)

type Client struct {
	Docs  *docs.Service
	Drive *drive.Service
}

func NewClient(ctx context.Context, cfg *config.Config) (*Client, error) {
	httpClient, err := SetupOAuth(cfg.CredentialsPath, cfg.TokenPath)
	if err != nil {
		return nil, err
	}

	opts := []option.ClientOption{
		option.WithHTTPClient(httpClient),
		option.WithUserAgent("org2gdocs-api/0.1.0"),
	}

	docsSvc, err := docs.NewService(ctx, opts...)
	if err != nil {
		return nil, err
	}

	driveSvc, err := drive.NewService(ctx, opts...)
	if err != nil {
		return nil, err
	}

	return &Client{Docs: docsSvc, Drive: driveSvc}, nil
}
