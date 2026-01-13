package main

import (
	"reflect"
	"testing"

	"org2gdocs/sexp"
)

func TestParseOperation_WithData(t *testing.T) {
	req := sexp.List{
		sexp.Symbol("operation"),
		sexp.Symbol("push"),
		sexp.Symbol(":data"),
		sexp.List{
			sexp.Symbol(":title"),
			sexp.String("Doc"),
		},
	}

	op, data, err := parseOperation(req)
	if err != nil {
		t.Fatalf("parseOperation() error: %v", err)
	}
	if op != "push" {
		t.Fatalf("op=%q, want %q", op, "push")
	}

	wantData := sexp.List{sexp.Symbol(":title"), sexp.String("Doc")}
	if !reflect.DeepEqual(data, wantData) {
		t.Fatalf("data mismatch\n got: %#v\nwant: %#v", data, wantData)
	}
}
