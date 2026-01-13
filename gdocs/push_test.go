package gdocs

import "testing"

func TestBuildHeadingRequests(t *testing.T) {
	reqs, newIndex, err := buildHeadingRequests(1, 2, "Hi")
	if err != nil {
		t.Fatalf("buildHeadingRequests() error: %v", err)
	}
	if newIndex != 4 {
		t.Fatalf("newIndex=%d, want 4", newIndex)
	}
	if len(reqs) != 2 {
		t.Fatalf("len(reqs)=%d, want 2", len(reqs))
	}
	if reqs[0].InsertText == nil || reqs[0].InsertText.Location.Index != 1 || reqs[0].InsertText.Text != "Hi\n" {
		t.Fatalf("unexpected insertText request: %#v", reqs[0].InsertText)
	}
	if reqs[1].UpdateParagraphStyle == nil || reqs[1].UpdateParagraphStyle.ParagraphStyle.NamedStyleType != "HEADING_2" {
		t.Fatalf("unexpected paragraph style request: %#v", reqs[1].UpdateParagraphStyle)
	}
}

func TestBuildParagraphRequests_WithBoldFormatting(t *testing.T) {
	reqs, newIndex, err := buildParagraphRequests(5, "abcd", []FormatRange{
		{Type: FormatBold, Start: 0, End: 2},
	})
	if err != nil {
		t.Fatalf("buildParagraphRequests() error: %v", err)
	}
	if newIndex != 10 {
		t.Fatalf("newIndex=%d, want 10", newIndex)
	}

	// InsertText + UpdateTextStyle
	if len(reqs) != 2 {
		t.Fatalf("len(reqs)=%d, want 2", len(reqs))
	}
	if reqs[1].UpdateTextStyle == nil {
		t.Fatalf("expected UpdateTextStyle request, got %#v", reqs[1])
	}
	if reqs[1].UpdateTextStyle.Range.StartIndex != 5 || reqs[1].UpdateTextStyle.Range.EndIndex != 7 {
		t.Fatalf("unexpected style range: %#v", reqs[1].UpdateTextStyle.Range)
	}
	if !reqs[1].UpdateTextStyle.TextStyle.Bold {
		t.Fatalf("expected bold=true")
	}
}

func TestBuildListRequests_Unordered(t *testing.T) {
	reqs, newIndex, err := buildListRequests(1, ListUnordered, []string{"A", "B"})
	if err != nil {
		t.Fatalf("buildListRequests() error: %v", err)
	}
	if newIndex != 5 {
		t.Fatalf("newIndex=%d, want 5", newIndex)
	}
	if len(reqs) != 2 {
		t.Fatalf("len(reqs)=%d, want 2", len(reqs))
	}
	if reqs[1].CreateParagraphBullets == nil || reqs[1].CreateParagraphBullets.BulletPreset != "BULLET_DISC_CIRCLE_SQUARE" {
		t.Fatalf("unexpected bullets request: %#v", reqs[1].CreateParagraphBullets)
	}
}

func TestBuildTextStyleRequests_Code(t *testing.T) {
	reqs, err := BuildTextStyleRequests("x", 1, []FormatRange{{Type: FormatCode, Start: 0, End: 1}})
	if err != nil {
		t.Fatalf("BuildTextStyleRequests() error: %v", err)
	}
	if len(reqs) != 1 || reqs[0].UpdateTextStyle == nil {
		t.Fatalf("unexpected requests: %#v", reqs)
	}
	if reqs[0].UpdateTextStyle.TextStyle.WeightedFontFamily == nil || reqs[0].UpdateTextStyle.TextStyle.WeightedFontFamily.FontFamily != "Courier New" {
		t.Fatalf("unexpected code style: %#v", reqs[0].UpdateTextStyle.TextStyle.WeightedFontFamily)
	}
}
