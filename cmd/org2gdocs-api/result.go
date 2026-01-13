package main

import (
	"fmt"
	"io"
	"time"

	"org2gdocs/sexp"
)

type errorCode string

const (
	errCodeParseError     errorCode = "parse_error"
	errCodeInvalidRequest errorCode = "invalid_request"
	errCodeNotImplemented errorCode = "not_implemented"
	errCodeAuthError      errorCode = "auth_error"
	errCodeConfigError    errorCode = "config_error"
	errCodeInternalError  errorCode = "internal_error"
)

func writeResult(w io.Writer, expr sexp.Sexp) error {
	out, err := sexp.Sprint(expr)
	if err != nil {
		return err
	}
	_, err = io.WriteString(w, out)
	if err != nil {
		return err
	}
	_, err = io.WriteString(w, "\n")
	return err
}

func writeErrorResult(w io.Writer, code errorCode, message string, details error) error {
	result := sexp.List{
		sexp.Symbol("result"),
		sexp.Symbol("error"),
		sexp.Symbol(":code"),
		sexp.Symbol(code),
		sexp.Symbol(":message"),
		sexp.String(message),
		sexp.Symbol(":timestamp"),
		sexp.String(time.Now().UTC().Format(time.RFC3339)),
	}

	if details != nil {
		result = append(result,
			sexp.Symbol(":details"),
			sexp.String(details.Error()),
		)
	}

	return writeResult(w, result)
}

func writeSuccessResult(w io.Writer, plist sexp.List) error {
	// plist should contain alternating keyword/value pairs.
	result := sexp.List{
		sexp.Symbol("result"),
		sexp.Symbol("success"),
	}

	if len(plist) > 0 {
		// Ensure caller passed keywords in correct shape. This is best-effort.
		if len(plist)%2 != 0 {
			return fmt.Errorf("success plist must have even length, got %d", len(plist))
		}
		result = append(result, plist...)
	}

	result = append(result,
		sexp.Symbol(":timestamp"),
		sexp.String(time.Now().UTC().Format(time.RFC3339)),
	)

	return writeResult(w, result)
}
