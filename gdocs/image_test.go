package gdocs

import (
	"bytes"
	"image"
	"image/color"
	"image/png"
	"os"
	"path/filepath"
	"testing"
)

func TestReadAndNormalizeImage_SVGToPNG(t *testing.T) {
	tmp := t.TempDir()
	svgPath := filepath.Join(tmp, "test.svg")

	svg := `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><rect width="10" height="10" fill="red"/></svg>`
	if err := os.WriteFile(svgPath, []byte(svg), 0o644); err != nil {
		t.Fatalf("write svg: %v", err)
	}

	b, err := readAndNormalizeImage(svgPath)
	if err != nil {
		t.Fatalf("readAndNormalizeImage() error: %v", err)
	}

	// PNG signature.
	if len(b) < 8 || !bytes.Equal(b[:8], []byte{0x89, 'P', 'N', 'G', 0x0D, 0x0A, 0x1A, 0x0A}) {
		t.Fatalf("expected PNG signature, got %x", b[:8])
	}
}

func TestReadAndNormalizeImage_PNGPassThrough(t *testing.T) {
	tmp := t.TempDir()
	pngPath := filepath.Join(tmp, "test.png")

	img := image.NewRGBA(image.Rect(0, 0, 1, 1))
	img.Set(0, 0, color.RGBA{R: 255, A: 255})

	var buf bytes.Buffer
	if err := png.Encode(&buf, img); err != nil {
		t.Fatalf("png encode: %v", err)
	}

	if err := os.WriteFile(pngPath, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write png: %v", err)
	}

	b, err := readAndNormalizeImage(pngPath)
	if err != nil {
		t.Fatalf("readAndNormalizeImage() error: %v", err)
	}

	if !bytes.Equal(b, buf.Bytes()) {
		t.Fatalf("png bytes mismatch")
	}
}
