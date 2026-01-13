package main

import (
	"context"
	"fmt"
	"io"
	"os"

	"org2gdocs/config"
	"org2gdocs/debug"
	"org2gdocs/gdocs"
	"org2gdocs/sexp"
)

func handleOperation(cfg *config.Config) {
	raw, err := io.ReadAll(os.Stdin)
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInternalError, "failed reading stdin", err)
		os.Exit(1)
	}

	debug.LogSexp("IN", string(raw))

	req, err := sexp.Parse(string(raw))
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeParseError, "failed to parse s-expression", err)
		os.Exit(1)
	}

	op, data, err := parseOperation(req)
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInvalidRequest, "invalid request", err)
		os.Exit(1)
	}

	debug.Log("Operation: %s", op)
	if data != nil {
		if s, err := sexp.Sprint(data); err == nil {
			debug.LogSexp("DATA", s)
		}
	}

	switch op {
	case "push":
		exit := handlePush(context.Background(), cfg, data)
		os.Exit(exit)
	case "pull":
		_ = writeErrorResult(os.Stdout, errCodeNotImplemented, "pull not implemented", nil)
		os.Exit(1)
	default:
		_ = writeErrorResult(os.Stdout, errCodeInvalidRequest, fmt.Sprintf("unknown operation: %s", op), nil)
		os.Exit(1)
	}
}

func parseOperation(expr sexp.Sexp) (op string, data sexp.Sexp, err error) {
	list, ok := expr.(sexp.List)
	if !ok || len(list) < 2 {
		return "", nil, fmt.Errorf("expected (operation <name> ...)")
	}

	head, ok := list[0].(sexp.Symbol)
	if !ok || head != "operation" {
		return "", nil, fmt.Errorf("expected first symbol 'operation'")
	}

	nameSym, ok := list[1].(sexp.Symbol)
	if !ok {
		return "", nil, fmt.Errorf("expected operation name symbol")
	}

	// Optional: :data <plist>
	for i := 2; i+1 < len(list); i += 2 {
		key, ok := list[i].(sexp.Symbol)
		if !ok {
			// Not a plist style request; ignore for now.
			break
		}
		if key == ":data" {
			return string(nameSym), list[i+1], nil
		}
	}

	return string(nameSym), nil, nil
}

func newGdocsClient(ctx context.Context, cfg *config.Config) (*gdocs.Client, error) {
	return gdocs.NewClient(ctx, cfg)
}
