package main

import (
	"fmt"
	"strings"

	"org2gdocs/sexp"
)

func decodePlist(expr sexp.Sexp) (map[sexp.Symbol]sexp.Sexp, error) {
	list, ok := expr.(sexp.List)
	if !ok {
		return nil, fmt.Errorf("expected plist list, got %T", expr)
	}
	if len(list)%2 != 0 {
		return nil, fmt.Errorf("plist must have even length, got %d", len(list))
	}

	out := make(map[sexp.Symbol]sexp.Sexp, len(list)/2)
	for i := 0; i < len(list); i += 2 {
		key, ok := list[i].(sexp.Symbol)
		if !ok || !strings.HasPrefix(string(key), ":") {
			return nil, fmt.Errorf("plist key must be keyword symbol, got %T (%v)", list[i], list[i])
		}
		out[key] = list[i+1]
	}
	return out, nil
}

func decodeString(expr sexp.Sexp) (string, error) {
	switch v := expr.(type) {
	case sexp.String:
		return string(v), nil
	case sexp.Symbol:
		// Accept bare symbols for convenience (e.g., ordered/unordered).
		return string(v), nil
	default:
		return "", fmt.Errorf("expected string/symbol, got %T", expr)
	}
}

func decodeOptionalString(expr sexp.Sexp) (string, error) {
	if expr == nil {
		return "", nil
	}
	if sym, ok := expr.(sexp.Symbol); ok && sym == "nil" {
		return "", nil
	}
	return decodeString(expr)
}

func decodeInt(expr sexp.Sexp) (int, error) {
	switch v := expr.(type) {
	case sexp.Number:
		return int(v), nil
	case int:
		return v, nil
	case int64:
		return int(v), nil
	default:
		return 0, fmt.Errorf("expected number, got %T", expr)
	}
}

func decodeStringList(expr sexp.Sexp) ([]string, error) {
	if expr == nil {
		return nil, nil
	}
	if sym, ok := expr.(sexp.Symbol); ok && sym == "nil" {
		return nil, nil
	}
	list, ok := expr.(sexp.List)
	if !ok {
		return nil, fmt.Errorf("expected list, got %T", expr)
	}
	out := make([]string, 0, len(list))
	for _, item := range list {
		s, err := decodeString(item)
		if err != nil {
			return nil, err
		}
		out = append(out, s)
	}
	return out, nil
}
