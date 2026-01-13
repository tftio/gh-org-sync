package sexp

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"

	"org2gdocs/debug"
)

type Sexp interface{}

type List []Sexp
type Symbol string
type String string
type Number int64

// Cons represents a cons cell / dotted pair.
//
// Example: (a . b)
type Cons struct {
	Car Sexp
	Cdr Sexp
}

var ErrUnexpectedEOF = errors.New("unexpected EOF")

func Parse(input string) (Sexp, error) {
	debug.Log("Parsing S-expression (length: %d bytes)", len(input))

	p := &parser{s: input}
	expr, err := p.parseExpr()
	if err != nil {
		debug.Log("Parse error: %v", err)
		return nil, err
	}

	// Ensure there's no trailing non-whitespace data.
	if tok, _ := p.peekToken(); tok.typ != tokEOF {
		return nil, fmt.Errorf("trailing data at byte %d", tok.pos)
	}

	return expr, nil
}

func ParseReader(r io.Reader) (Sexp, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

func Sprint(expr Sexp) (string, error) {
	var buf bytes.Buffer
	if err := Write(&buf, expr); err != nil {
		return "", err
	}
	return buf.String(), nil
}

func Write(w io.Writer, expr Sexp) error {
	var buf bytes.Buffer
	if err := writeExpr(&buf, expr); err != nil {
		return err
	}
	_, err := w.Write(buf.Bytes())
	return err
}

func writeExpr(buf *bytes.Buffer, expr Sexp) error {
	switch v := expr.(type) {
	case nil:
		buf.WriteString("nil")
		return nil
	case Symbol:
		buf.WriteString(string(v))
		return nil
	case String:
		writeString(buf, string(v))
		return nil
	case Number:
		buf.WriteString(strconv.FormatInt(int64(v), 10))
		return nil
	case int:
		buf.WriteString(strconv.FormatInt(int64(v), 10))
		return nil
	case int64:
		buf.WriteString(strconv.FormatInt(v, 10))
		return nil
	case List:
		buf.WriteByte('(')
		for i, item := range v {
			if i > 0 {
				buf.WriteByte(' ')
			}
			if err := writeExpr(buf, item); err != nil {
				return err
			}
		}
		buf.WriteByte(')')
		return nil
	case Cons:
		return writeCons(buf, v)
	default:
		return fmt.Errorf("unsupported s-expression type %T", expr)
	}
}

func writeCons(buf *bytes.Buffer, c Cons) error {
	buf.WriteByte('(')
	if err := writeExpr(buf, c.Car); err != nil {
		return err
	}

	tail := c.Cdr
	for {
		next, ok := tail.(Cons)
		if !ok {
			break
		}
		buf.WriteByte(' ')
		if err := writeExpr(buf, next.Car); err != nil {
			return err
		}
		tail = next.Cdr
	}

	// If tail is nil, treat as proper list.
	if tail == nil {
		buf.WriteByte(')')
		return nil
	}

	if sym, ok := tail.(Symbol); ok && sym == "nil" {
		buf.WriteByte(')')
		return nil
	}

	buf.WriteString(" . ")
	if err := writeExpr(buf, tail); err != nil {
		return err
	}
	buf.WriteByte(')')
	return nil
}

func writeString(buf *bytes.Buffer, s string) {
	buf.WriteByte('"')
	for _, r := range s {
		switch r {
		case '\\':
			buf.WriteString(`\\`)
		case '"':
			buf.WriteString(`\"`)
		case '\n':
			buf.WriteString(`\n`)
		case '\t':
			buf.WriteString(`\t`)
		case '\r':
			buf.WriteString(`\r`)
		default:
			buf.WriteRune(r)
		}
	}
	buf.WriteByte('"')
}

// --- Parser implementation ---

type tokenType int

const (
	tokEOF tokenType = iota
	tokLParen
	tokRParen
	tokDot
	tokQuote
	tokString
	tokSymbol
	tokNumber
)

func (t tokenType) String() string {
	switch t {
	case tokEOF:
		return "EOF"
	case tokLParen:
		return "("
	case tokRParen:
		return ")"
	case tokDot:
		return "."
	case tokQuote:
		return "'"
	case tokString:
		return "string"
	case tokSymbol:
		return "symbol"
	case tokNumber:
		return "number"
	default:
		return fmt.Sprintf("token(%d)", int(t))
	}
}

type token struct {
	typ tokenType
	val string
	pos int // byte offset
}

type parser struct {
	s   string
	i   int // byte offset
	buf *token
}

func (p *parser) parseExpr() (Sexp, error) {
	tok, err := p.nextToken()
	if err != nil {
		return nil, err
	}

	switch tok.typ {
	case tokEOF:
		return nil, ErrUnexpectedEOF
	case tokLParen:
		return p.parseList()
	case tokRParen:
		return nil, fmt.Errorf("unexpected ')', byte %d", tok.pos)
	case tokDot:
		return nil, fmt.Errorf("unexpected '.', byte %d", tok.pos)
	case tokQuote:
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return List{Symbol("quote"), expr}, nil
	case tokString:
		return String(tok.val), nil
	case tokNumber:
		n, err := strconv.ParseInt(tok.val, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("invalid integer %q at byte %d", tok.val, tok.pos)
		}
		return Number(n), nil
	case tokSymbol:
		return Symbol(tok.val), nil
	default:
		return nil, fmt.Errorf("unexpected token type %d at byte %d", tok.typ, tok.pos)
	}
}

func (p *parser) parseList() (Sexp, error) {
	var elems []Sexp

	for {
		tok, err := p.peekToken()
		if err != nil {
			return nil, err
		}

		switch tok.typ {
		case tokEOF:
			return nil, ErrUnexpectedEOF
		case tokRParen:
			_, _ = p.nextToken() // consume ')'
			return List(elems), nil
		case tokDot:
			_, _ = p.nextToken() // consume '.'

			tail, err := p.parseExpr()
			if err != nil {
				return nil, err
			}

			end, err := p.nextToken()
			if err != nil {
				return nil, err
			}
			if end.typ != tokRParen {
				return nil, fmt.Errorf("expected ')', got %s at byte %d", end.typ, end.pos)
			}

			// Build nested cons cells from the head elements to the tail.
			out := tail
			for i := len(elems) - 1; i >= 0; i-- {
				out = Cons{Car: elems[i], Cdr: out}
			}
			return out, nil
		default:
			expr, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			elems = append(elems, expr)
		}
	}
}

func (p *parser) peekToken() (token, error) {
	if p.buf != nil {
		return *p.buf, nil
	}
	tok, err := p.nextTokenRaw()
	if err != nil {
		return tok, err
	}
	p.buf = &tok
	return tok, nil
}

func (p *parser) nextToken() (token, error) {
	if p.buf != nil {
		tok := *p.buf
		p.buf = nil
		return tok, nil
	}
	return p.nextTokenRaw()
}

func (p *parser) nextTokenRaw() (token, error) {
	p.skipWhitespaceAndComments()
	if p.i >= len(p.s) {
		return token{typ: tokEOF, pos: p.i}, nil
	}

	start := p.i
	ch := p.s[p.i]

	switch ch {
	case '(':
		p.i++
		return token{typ: tokLParen, pos: start}, nil
	case ')':
		p.i++
		return token{typ: tokRParen, pos: start}, nil
	case '\'':
		p.i++
		return token{typ: tokQuote, pos: start}, nil
	case '.':
		// Dot is a token only when it stands alone: (a . b)
		if p.i+1 == len(p.s) || isDelimiter(p.s[p.i+1]) {
			p.i++
			return token{typ: tokDot, pos: start}, nil
		}
	}

	if ch == '"' {
		str, err := p.readString()
		if err != nil {
			return token{}, err
		}
		return token{typ: tokString, val: str, pos: start}, nil
	}

	// Read a symbol/number token.
	for p.i < len(p.s) && !isDelimiter(p.s[p.i]) {
		p.i++
	}
	lit := p.s[start:p.i]

	if isIntegerLiteral(lit) {
		return token{typ: tokNumber, val: lit, pos: start}, nil
	}
	return token{typ: tokSymbol, val: lit, pos: start}, nil
}

func (p *parser) skipWhitespaceAndComments() {
	for p.i < len(p.s) {
		ch := p.s[p.i]
		if isWhitespace(ch) {
			p.i++
			continue
		}
		if ch == ';' {
			// Comment to end of line.
			for p.i < len(p.s) && p.s[p.i] != '\n' {
				p.i++
			}
			continue
		}
		break
	}
}

func (p *parser) readString() (string, error) {
	// Leading quote must be present.
	if p.i >= len(p.s) || p.s[p.i] != '"' {
		return "", fmt.Errorf("internal error: expected string at byte %d", p.i)
	}
	p.i++ // consume opening quote

	var b strings.Builder
	for {
		if p.i >= len(p.s) {
			return "", ErrUnexpectedEOF
		}

		ch := p.s[p.i]
		p.i++

		switch ch {
		case '"':
			return b.String(), nil
		case '\\':
			if p.i >= len(p.s) {
				return "", ErrUnexpectedEOF
			}
			esc := p.s[p.i]
			p.i++
			switch esc {
			case '\\', '"':
				b.WriteByte(esc)
			case 'n':
				b.WriteByte('\n')
			case 't':
				b.WriteByte('\t')
			case 'r':
				b.WriteByte('\r')
			case 'x':
				// \xHH
				if p.i+2 > len(p.s) {
					return "", ErrUnexpectedEOF
				}
				v, err := strconv.ParseUint(p.s[p.i:p.i+2], 16, 8)
				if err != nil {
					return "", fmt.Errorf("invalid \\x escape at byte %d", p.i-2)
				}
				b.WriteByte(byte(v))
				p.i += 2
			case 'u':
				// \uHHHH
				if p.i+4 > len(p.s) {
					return "", ErrUnexpectedEOF
				}
				v, err := strconv.ParseUint(p.s[p.i:p.i+4], 16, 16)
				if err != nil {
					return "", fmt.Errorf("invalid \\u escape at byte %d", p.i-2)
				}
				b.WriteRune(rune(v))
				p.i += 4
			case 'U':
				// \UHHHHHHHH
				if p.i+8 > len(p.s) {
					return "", ErrUnexpectedEOF
				}
				v, err := strconv.ParseUint(p.s[p.i:p.i+8], 16, 32)
				if err != nil {
					return "", fmt.Errorf("invalid \\U escape at byte %d", p.i-2)
				}
				b.WriteRune(rune(v))
				p.i += 8
			default:
				// Unknown escape: preserve literal character.
				b.WriteByte(esc)
			}
		default:
			b.WriteByte(ch)
		}
	}
}

func isWhitespace(ch byte) bool {
	switch ch {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func isDelimiter(ch byte) bool {
	if isWhitespace(ch) {
		return true
	}
	switch ch {
	case '(', ')', '"', ';', '\'':
		return true
	default:
		return false
	}
}

func isIntegerLiteral(s string) bool {
	if s == "" {
		return false
	}
	// Allow leading sign.
	if s[0] == '+' || s[0] == '-' {
		if len(s) == 1 {
			return false
		}
		s = s[1:]
	}
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}
