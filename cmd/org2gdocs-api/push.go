package main

import (
	"context"
	"fmt"
	"os"
	"regexp"
	"sort"

	"org2gdocs/config"
	"org2gdocs/gdocs"
	"org2gdocs/sexp"
)

type pushRequest struct {
	DocumentID        string
	Title             string
	Content           []gdocs.Content
	CommentsToResolve []string
}

var commentMarkerRe = regexp.MustCompile(`\[\[id:comment-[^]]+\]\[•\]\]`)

func handlePush(ctx context.Context, cfg *config.Config, data sexp.Sexp) int {
	req, err := decodePushRequest(data)
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInvalidRequest, "invalid push request", err)
		return 1
	}

	client, err := newGdocsClient(ctx, cfg)
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeAuthError, "failed to authenticate", err)
		return 1
	}

	result, err := gdocs.PushDocument(ctx, client, req.DocumentID, req.Title, req.Content)
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInternalError, "push failed", err)
		return 1
	}

	plist := sexp.List{
		sexp.Symbol(":document-id"),
		sexp.String(result.DocumentID),
		sexp.Symbol(":document-url"),
		sexp.String(result.DocumentURL),
		sexp.Symbol(":position-map"),
		encodePositionMap(result.PositionMap),
		sexp.Symbol(":resolved-comments"),
		sexp.List{},
	}

	if err := writeSuccessResult(os.Stdout, plist); err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInternalError, "failed to write response", err)
		return 1
	}
	return 0
}

func decodePushRequest(expr sexp.Sexp) (*pushRequest, error) {
	plist, err := decodePlist(expr)
	if err != nil {
		return nil, err
	}

	docID, err := decodeOptionalString(plist[sexp.Symbol(":document-id")])
	if err != nil {
		return nil, fmt.Errorf(":document-id: %w", err)
	}

	title, err := decodeString(plist[sexp.Symbol(":title")])
	if err != nil {
		return nil, fmt.Errorf(":title: %w", err)
	}

	contentExpr, ok := plist[sexp.Symbol(":content")]
	if !ok {
		return nil, fmt.Errorf("missing :content")
	}

	content, err := decodeContentList(contentExpr)
	if err != nil {
		return nil, fmt.Errorf(":content: %w", err)
	}

	commentsToResolve, err := decodeStringList(plist[sexp.Symbol(":comments-to-resolve")])
	if err != nil {
		return nil, fmt.Errorf(":comments-to-resolve: %w", err)
	}

	return &pushRequest{
		DocumentID:        docID,
		Title:             title,
		Content:           content,
		CommentsToResolve: commentsToResolve,
	}, nil
}

func decodeContentList(expr sexp.Sexp) ([]gdocs.Content, error) {
	list, ok := expr.(sexp.List)
	if !ok {
		return nil, fmt.Errorf("expected list, got %T", expr)
	}

	out := make([]gdocs.Content, 0, len(list))
	for _, item := range list {
		c, err := decodeContentItem(item)
		if err != nil {
			return nil, err
		}
		out = append(out, c)
	}
	return out, nil
}

func decodeContentItem(expr sexp.Sexp) (gdocs.Content, error) {
	list, ok := expr.(sexp.List)
	if !ok || len(list) == 0 {
		return gdocs.Content{}, fmt.Errorf("content item must be list, got %T", expr)
	}

	typSym, ok := list[0].(sexp.Symbol)
	if !ok {
		return gdocs.Content{}, fmt.Errorf("content item type must be symbol, got %T", list[0])
	}

	props, err := decodePlist(sexp.List(list[1:]))
	if err != nil {
		return gdocs.Content{}, err
	}

	contentType := gdocs.ContentType(typSym)
	switch contentType {
	case gdocs.ContentHeading:
		level, err := decodeInt(props[sexp.Symbol(":level")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("heading :level: %w", err)
		}
		text, err := decodeString(props[sexp.Symbol(":text")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("heading :text: %w", err)
		}
		customID, err := decodeString(props[sexp.Symbol(":custom-id")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("heading :custom-id: %w", err)
		}

		return gdocs.Content{
			Type:     contentType,
			Level:    level,
			Text:     stripCommentMarkers(text),
			CustomID: customID,
		}, nil

	case gdocs.ContentParagraph:
		text, err := decodeString(props[sexp.Symbol(":text")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("paragraph :text: %w", err)
		}
		customID, err := decodeString(props[sexp.Symbol(":custom-id")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("paragraph :custom-id: %w", err)
		}

		formatting, err := decodeFormatting(props[sexp.Symbol(":formatting")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("paragraph :formatting: %w", err)
		}

		return gdocs.Content{
			Type:       contentType,
			Text:       stripCommentMarkers(text),
			CustomID:   customID,
			Formatting: formatting,
		}, nil

	case gdocs.ContentList:
		listTypeStr, err := decodeString(props[sexp.Symbol(":type")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("list :type: %w", err)
		}
		customID, err := decodeString(props[sexp.Symbol(":custom-id")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("list :custom-id: %w", err)
		}
		items, err := decodeStringList(props[sexp.Symbol(":items")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("list :items: %w", err)
		}

		for i := range items {
			items[i] = stripCommentMarkers(items[i])
		}

		return gdocs.Content{
			Type:     contentType,
			ListType: gdocs.ListType(listTypeStr),
			Items:    items,
			CustomID: customID,
		}, nil

	case gdocs.ContentTable:
		customID, err := decodeString(props[sexp.Symbol(":custom-id")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("table :custom-id: %w", err)
		}
		rows, err := decodeTableRows(props[sexp.Symbol(":rows")])
		if err != nil {
			return gdocs.Content{}, fmt.Errorf("table :rows: %w", err)
		}
		for r := range rows {
			for c := range rows[r] {
				rows[r][c] = stripCommentMarkers(rows[r][c])
			}
		}

		return gdocs.Content{
			Type:     contentType,
			CustomID: customID,
			Rows:     rows,
		}, nil

	case gdocs.ContentImage:
		return gdocs.Content{}, fmt.Errorf("image content requires Task 3")

	default:
		return gdocs.Content{}, fmt.Errorf("unsupported content type: %q", contentType)
	}
}

func decodeFormatting(expr sexp.Sexp) ([]gdocs.FormatRange, error) {
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

	out := make([]gdocs.FormatRange, 0, len(list))
	for _, item := range list {
		itemList, ok := item.(sexp.List)
		if !ok || len(itemList) == 0 {
			return nil, fmt.Errorf("format item must be list, got %T", item)
		}

		typeSym, ok := itemList[0].(sexp.Symbol)
		if !ok {
			return nil, fmt.Errorf("format item type must be symbol, got %T", itemList[0])
		}
		props, err := decodePlist(sexp.List(itemList[1:]))
		if err != nil {
			return nil, err
		}

		start, err := decodeInt(props[sexp.Symbol(":start")])
		if err != nil {
			return nil, fmt.Errorf("format :start: %w", err)
		}
		end, err := decodeInt(props[sexp.Symbol(":end")])
		if err != nil {
			return nil, fmt.Errorf("format :end: %w", err)
		}

		var url string
		if urlExpr, ok := props[sexp.Symbol(":url")]; ok {
			url, err = decodeOptionalString(urlExpr)
			if err != nil {
				return nil, fmt.Errorf("format :url: %w", err)
			}
		}

		out = append(out, gdocs.FormatRange{
			Type:  gdocs.FormatType(typeSym),
			Start: start,
			End:   end,
			URL:   url,
		})
	}

	return out, nil
}

func decodeTableRows(expr sexp.Sexp) ([][]string, error) {
	list, ok := expr.(sexp.List)
	if !ok {
		return nil, fmt.Errorf("expected rows list, got %T", expr)
	}

	out := make([][]string, 0, len(list))
	for _, rowExpr := range list {
		rowList, ok := rowExpr.(sexp.List)
		if !ok {
			return nil, fmt.Errorf("row must be list, got %T", rowExpr)
		}
		row := make([]string, 0, len(rowList))
		for _, cell := range rowList {
			s, err := decodeString(cell)
			if err != nil {
				return nil, err
			}
			row = append(row, s)
		}
		out = append(out, row)
	}
	return out, nil
}

func stripCommentMarkers(s string) string {
	return commentMarkerRe.ReplaceAllString(s, "")
}

func encodePositionMap(posMap map[string]gdocs.Position) sexp.Sexp {
	keys := make([]string, 0, len(posMap))
	for k := range posMap {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	out := make(sexp.List, 0, len(keys))
	for _, k := range keys {
		pos := posMap[k]
		out = append(out, sexp.Cons{
			Car: sexp.String(k),
			Cdr: sexp.List{
				sexp.Symbol(":gdoc-index"),
				sexp.Number(pos.GdocIndex),
				sexp.Symbol(":type"),
				sexp.Symbol(pos.Type),
			},
		})
	}
	return out
}
