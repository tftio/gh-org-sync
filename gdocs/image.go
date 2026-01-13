package gdocs

import (
	"bytes"
	"context"
	"fmt"
	"image"
	"image/png"
	"mime"
	"os"
	"path/filepath"
	"strings"
	"time"

	isSvg "github.com/h2non/go-is-svg"
	"github.com/srwiley/oksvg"
	"github.com/srwiley/rasterx"
	"google.golang.org/api/docs/v1"
	"google.golang.org/api/drive/v3"
	"google.golang.org/api/googleapi"
)

func insertImage(ctx context.Context, c *Client, docID string, index int64, imagePath string, altText string) (int64, error) {
	imageBytes, err := readAndNormalizeImage(imagePath)
	if err != nil {
		return 0, err
	}

	fileID, publicURL, err := uploadPublicPNG(ctx, c.Drive, imageBytes)
	if err != nil {
		return 0, err
	}

	// Best-effort cleanup of the temporary Drive file.
	defer func() {
		_ = c.Drive.Files.Delete(fileID).Context(ctx).Do()
	}()

	// InsertInlineImage requires a publicly accessible URL.
	//
	// Also: insert a newline after the image so that the next insertion lands in
	// a fresh paragraph, consistent with how we handle text content.
	_, err = c.Docs.Documents.BatchUpdate(docID, &docs.BatchUpdateDocumentRequest{
		Requests: []*docs.Request{
			{
				InsertInlineImage: &docs.InsertInlineImageRequest{
					Location: &docs.Location{Index: index},
					Uri:      publicURL,
				},
			},
			{
				InsertText: &docs.InsertTextRequest{
					Location: &docs.Location{Index: index + 1},
					Text:     "\n",
				},
			},
		},
	}).Context(ctx).Do()
	if err != nil {
		return 0, err
	}

	// Inline objects occupy a single index position (similar to an embedded
	// character). We also inserted a newline, so advance by 2.
	return index + 2, nil
}

func readAndNormalizeImage(path string) ([]byte, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	ext := strings.ToLower(filepath.Ext(path))
	switch {
	case ext == ".svg" || isSvg.Is(b):
		return convertSVGToPNG(b)
	case ext == ".png":
		return b, nil
	default:
		m := mime.TypeByExtension(ext)
		if m == "" {
			m = "unknown"
		}
		return nil, fmt.Errorf("unsupported image type %q (%s): %s", ext, m, path)
	}
}

func convertSVGToPNG(svgData []byte) ([]byte, error) {
	icon, err := oksvg.ReadIconStream(bytes.NewReader(svgData))
	if err != nil {
		return nil, err
	}

	// Prefer explicit viewbox, else fall back to a reasonable default.
	w := int(icon.ViewBox.W)
	h := int(icon.ViewBox.H)
	if w <= 0 {
		w = 512
	}
	if h <= 0 {
		h = 512
	}

	img := image.NewRGBA(image.Rect(0, 0, w, h))
	scanner := rasterx.NewScannerGV(w, h, img, img.Bounds())
	raster := rasterx.NewDasher(w, h, scanner)
	icon.SetTarget(0, 0, float64(w), float64(h))
	icon.Draw(raster, 1.0)

	var buf bytes.Buffer
	if err := png.Encode(&buf, img); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func uploadPublicPNG(ctx context.Context, driveSvc *drive.Service, pngBytes []byte) (fileID string, publicURL string, err error) {
	name := fmt.Sprintf("org2gdocs-%d.png", time.Now().UTC().UnixNano())

	f, err := driveSvc.Files.Create(&drive.File{
		Name:     name,
		MimeType: "image/png",
	}).Media(bytes.NewReader(pngBytes), googleapi.ContentType("image/png")).Fields("id").Context(ctx).Do()
	if err != nil {
		return "", "", err
	}

	// Make the file publicly readable so Docs can fetch it.
	_, err = driveSvc.Permissions.Create(f.Id, &drive.Permission{
		Type: "anyone",
		Role: "reader",
	}).Context(ctx).Do()
	if err != nil {
		return "", "", err
	}

	// Public download URL.
	publicURL = fmt.Sprintf("https://drive.google.com/uc?export=download&id=%s", f.Id)
	return f.Id, publicURL, nil
}
