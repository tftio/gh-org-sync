package main

import (
	"reflect"
	"testing"

	"org2gdocs/gdocs"
	"org2gdocs/sexp"
)

func TestDecodePushRequest_StripsCommentMarkers(t *testing.T) {
	data := sexp.List{
		sexp.Symbol(":document-id"), sexp.Symbol("nil"),
		sexp.Symbol(":title"), sexp.String("Doc"),
		sexp.Symbol(":content"), sexp.List{
			sexp.List{
				sexp.Symbol("heading"),
				sexp.Symbol(":level"), sexp.Number(1),
				sexp.Symbol(":text"), sexp.String("Intro"),
				sexp.Symbol(":custom-id"), sexp.String("sec-intro"),
			},
			sexp.List{
				sexp.Symbol("paragraph"),
				sexp.Symbol(":text"), sexp.String("Hello[[id:comment-xyz][•]]world"),
				sexp.Symbol(":custom-id"), sexp.String("sec-intro/para-0"),
				sexp.Symbol(":formatting"), sexp.List{
					sexp.List{
						sexp.Symbol("bold"),
						sexp.Symbol(":start"), sexp.Number(0),
						sexp.Symbol(":end"), sexp.Number(5),
					},
				},
			},
		},
		sexp.Symbol(":comments-to-resolve"), sexp.List{sexp.String("c1")},
	}

	req, err := decodePushRequest(data)
	if err != nil {
		t.Fatalf("decodePushRequest() error: %v", err)
	}

	if req.DocumentID != "" {
		t.Fatalf("DocumentID=%q, want empty", req.DocumentID)
	}
	if req.Title != "Doc" {
		t.Fatalf("Title=%q, want %q", req.Title, "Doc")
	}
	if len(req.Content) != 2 {
		t.Fatalf("len(Content)=%d, want 2", len(req.Content))
	}

	para := req.Content[1]
	if para.Type != gdocs.ContentParagraph {
		t.Fatalf("para.Type=%q, want %q", para.Type, gdocs.ContentParagraph)
	}
	if para.Text != "Helloworld" {
		t.Fatalf("para.Text=%q, want %q", para.Text, "Helloworld")
	}
	if len(para.Formatting) != 1 || para.Formatting[0].Type != gdocs.FormatBold {
		t.Fatalf("unexpected formatting: %#v", para.Formatting)
	}
}

func TestEncodePositionMap_SortsKeys(t *testing.T) {
	pos := map[string]gdocs.Position{
		"b": {GdocIndex: 2, Type: gdocs.ContentParagraph},
		"a": {GdocIndex: 1, Type: gdocs.ContentHeading},
	}

	encoded := encodePositionMap(pos)
	list, ok := encoded.(sexp.List)
	if !ok {
		t.Fatalf("expected list, got %T", encoded)
	}
	if len(list) != 2 {
		t.Fatalf("len(list)=%d, want 2", len(list))
	}

	first, ok := list[0].(sexp.Cons)
	if !ok {
		t.Fatalf("expected cons at list[0], got %T", list[0])
	}

	if first.Car != sexp.String("a") {
		t.Fatalf("first key=%#v, want %q", first.Car, "a")
	}

	// Spot-check the second entry for ordering and shape.
	second, ok := list[1].(sexp.Cons)
	if !ok {
		t.Fatalf("expected cons at list[1], got %T", list[1])
	}
	if second.Car != sexp.String("b") {
		t.Fatalf("second key=%#v, want %q", second.Car, "b")
	}

	wantCdr := sexp.List{
		sexp.Symbol(":gdoc-index"),
		sexp.Number(2),
		sexp.Symbol(":type"),
		sexp.Symbol(gdocs.ContentParagraph),
	}
	if !reflect.DeepEqual(second.Cdr, wantCdr) {
		t.Fatalf("cdr mismatch\n got: %#v\nwant: %#v", second.Cdr, wantCdr)
	}
}
