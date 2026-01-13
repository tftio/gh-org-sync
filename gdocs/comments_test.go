package gdocs

import "testing"

func TestParseAnchorStartEnd_TextRange(t *testing.T) {
	start, end, ok := parseAnchorStartEnd(`{"textRange":{"startIndex":10,"endIndex":42}}`)
	if !ok {
		t.Fatalf("expected ok")
	}
	if start != 10 || end != 42 {
		t.Fatalf("start/end=%d/%d, want 10/42", start, end)
	}
}

func TestMapAnchor_MapsToSegment(t *testing.T) {
	positions := []posEntry{
		{ID: "sec-intro", Start: 1, Type: ContentHeading},
		{ID: "sec-intro/para-0", Start: 5, Type: ContentParagraph},
		{ID: "sec-next", Start: 20, Type: ContentHeading},
	}

	a := mapAnchor(`{"startIndex":6,"endIndex":9}`, "quoted", positions)
	if a.SegmentID != "sec-intro/para-0" {
		t.Fatalf("SegmentID=%q, want %q", a.SegmentID, "sec-intro/para-0")
	}
	if a.CustomID != "sec-intro" {
		t.Fatalf("CustomID=%q, want %q", a.CustomID, "sec-intro")
	}
	if a.StartIndex != 1 || a.EndIndex != 4 {
		t.Fatalf("StartIndex/EndIndex=%d/%d, want 1/4", a.StartIndex, a.EndIndex)
	}
	if a.QuotedText != "quoted" {
		t.Fatalf("QuotedText=%q, want %q", a.QuotedText, "quoted")
	}
}
