package main

import (
	"testing"

	"org2gdocs/gdocs"
	"org2gdocs/sexp"
)

func TestDecodePullRequest(t *testing.T) {
	data := sexp.List{
		sexp.Symbol(":document-id"), sexp.String("doc-1"),
		sexp.Symbol(":position-map"), sexp.List{
			sexp.Cons{
				Car: sexp.String("sec-intro"),
				Cdr: sexp.List{
					sexp.Symbol(":gdoc-index"), sexp.Number(1),
					sexp.Symbol(":type"), sexp.Symbol("heading"),
				},
			},
		},
		sexp.Symbol(":known-comment-ids"), sexp.List{sexp.String("c1")},
		sexp.Symbol(":collaborator-cache"), sexp.List{
			sexp.Cons{Car: sexp.String("a@example.com"), Cdr: sexp.String("A Example")},
		},
	}

	req, err := decodePullRequest(data)
	if err != nil {
		t.Fatalf("decodePullRequest() error: %v", err)
	}

	if req.DocumentID != "doc-1" {
		t.Fatalf("DocumentID=%q, want %q", req.DocumentID, "doc-1")
	}
	if _, ok := req.KnownCommentIDs["c1"]; !ok {
		t.Fatalf("expected known comment id c1")
	}
	if got := req.CollaboratorCache["a@example.com"]; got != "A Example" {
		t.Fatalf("CollaboratorCache[a@example.com]=%q, want %q", got, "A Example")
	}

	pos := req.PositionMap["sec-intro"]
	if pos.GdocIndex != 1 || pos.Type != gdocs.ContentHeading {
		t.Fatalf("PositionMap[sec-intro]=%#v, want index=1 type=heading", pos)
	}
}
