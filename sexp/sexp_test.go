package sexp

import (
	"reflect"
	"testing"
)

func TestParse_ListWithPlist(t *testing.T) {
	in := `(operation push :data (:document-id nil :title "Doc Title" :count 3))`
	got, err := Parse(in)
	if err != nil {
		t.Fatalf("Parse() error: %v", err)
	}

	want := List{
		Symbol("operation"),
		Symbol("push"),
		Symbol(":data"),
		List{
			Symbol(":document-id"),
			Symbol("nil"),
			Symbol(":title"),
			String("Doc Title"),
			Symbol(":count"),
			Number(3),
		},
	}

	if !reflect.DeepEqual(got, want) {
		t.Fatalf("unexpected parse result\n got: %#v\nwant: %#v", got, want)
	}
}

func TestParse_DottedPair(t *testing.T) {
	in := `("sec-intro" . (:gdoc-index 1 :type heading))`
	got, err := Parse(in)
	if err != nil {
		t.Fatalf("Parse() error: %v", err)
	}

	want := Cons{
		Car: String("sec-intro"),
		Cdr: List{
			Symbol(":gdoc-index"),
			Number(1),
			Symbol(":type"),
			Symbol("heading"),
		},
	}

	if !reflect.DeepEqual(got, want) {
		t.Fatalf("unexpected parse result\n got: %#v\nwant: %#v", got, want)
	}
}

func TestParse_Quote(t *testing.T) {
	in := `'("a")`
	got, err := Parse(in)
	if err != nil {
		t.Fatalf("Parse() error: %v", err)
	}

	want := List{Symbol("quote"), List{String("a")}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("unexpected parse result\n got: %#v\nwant: %#v", got, want)
	}
}

func TestParse_Comments(t *testing.T) {
	in := "(a ; hello\n b)"
	got, err := Parse(in)
	if err != nil {
		t.Fatalf("Parse() error: %v", err)
	}

	want := List{Symbol("a"), Symbol("b")}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("unexpected parse result\n got: %#v\nwant: %#v", got, want)
	}
}

func TestWrite_RoundTrip(t *testing.T) {
	in := List{
		Symbol("result"),
		Symbol("success"),
		Symbol(":message"),
		String("hello\nworld"),
	}

	wire, err := Sprint(in)
	if err != nil {
		t.Fatalf("Sprint() error: %v", err)
	}

	got, err := Parse(wire)
	if err != nil {
		t.Fatalf("Parse(roundtrip) error: %v\nwire: %s", err, wire)
	}

	if !reflect.DeepEqual(got, in) {
		t.Fatalf("roundtrip mismatch\n got: %#v\nwant: %#v\nwire: %s", got, in, wire)
	}
}
