package main

import (
	"context"
	"fmt"
	"os"
	"sort"

	"org2gdocs/config"
	"org2gdocs/gdocs"
	"org2gdocs/sexp"
)

type pullRequest struct {
	DocumentID        string
	PositionMap       map[string]gdocs.Position
	KnownCommentIDs   map[string]struct{}
	CollaboratorCache map[string]string
}

func handlePull(ctx context.Context, cfg *config.Config, data sexp.Sexp) int {
	req, err := decodePullRequest(data)
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInvalidRequest, "invalid pull request", err)
		return 1
	}

	client, err := newGdocsClient(ctx, cfg)
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeAuthError, "failed to authenticate", err)
		return 1
	}

	res, err := gdocs.Pull(ctx, client, gdocs.PullRequest{
		DocumentID:        req.DocumentID,
		PositionMap:       req.PositionMap,
		KnownCommentIDs:   req.KnownCommentIDs,
		CollaboratorCache: req.CollaboratorCache,
	})
	if err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInternalError, "pull failed", err)
		return 1
	}

	plist := sexp.List{
		sexp.Symbol(":document-id"),
		sexp.String(res.DocumentID),
		sexp.Symbol(":comments"),
		encodeComments(res.Comments),
		sexp.Symbol(":suggested-edits"),
		sexp.List{},
		sexp.Symbol(":new-collaborators"),
		encodeCollaborators(res.NewCollaborators),
	}

	if err := writeSuccessResult(os.Stdout, plist); err != nil {
		_ = writeErrorResult(os.Stdout, errCodeInternalError, "failed to write response", err)
		return 1
	}

	return 0
}

func decodePullRequest(expr sexp.Sexp) (*pullRequest, error) {
	plist, err := decodePlist(expr)
	if err != nil {
		return nil, err
	}

	docID, err := decodeString(plist[sexp.Symbol(":document-id")])
	if err != nil {
		return nil, fmt.Errorf(":document-id: %w", err)
	}
	if docID == "" {
		return nil, fmt.Errorf(":document-id is required")
	}

	posMap, err := decodePositionMap(plist[sexp.Symbol(":position-map")])
	if err != nil {
		return nil, fmt.Errorf(":position-map: %w", err)
	}

	knownIDs, err := decodeStringList(plist[sexp.Symbol(":known-comment-ids")])
	if err != nil {
		return nil, fmt.Errorf(":known-comment-ids: %w", err)
	}
	knownSet := make(map[string]struct{}, len(knownIDs))
	for _, id := range knownIDs {
		knownSet[id] = struct{}{}
	}

	collabCache, err := decodeCollaboratorCache(plist[sexp.Symbol(":collaborator-cache")])
	if err != nil {
		return nil, fmt.Errorf(":collaborator-cache: %w", err)
	}

	return &pullRequest{
		DocumentID:        docID,
		PositionMap:       posMap,
		KnownCommentIDs:   knownSet,
		CollaboratorCache: collabCache,
	}, nil
}

func decodePositionMap(expr sexp.Sexp) (map[string]gdocs.Position, error) {
	if expr == nil {
		return map[string]gdocs.Position{}, nil
	}
	if sym, ok := expr.(sexp.Symbol); ok && sym == "nil" {
		return map[string]gdocs.Position{}, nil
	}

	list, ok := expr.(sexp.List)
	if !ok {
		return nil, fmt.Errorf("expected list, got %T", expr)
	}

	out := make(map[string]gdocs.Position, len(list))
	for _, item := range list {
		var key string
		var plist map[sexp.Symbol]sexp.Sexp
		var err error

		switch v := item.(type) {
		case sexp.Cons:
			// Dotted pair format: ("key" . (:k1 v1 :k2 v2))
			key, err = decodeString(v.Car)
			if err != nil {
				return nil, fmt.Errorf("position-map key: %w", err)
			}
			plist, err = decodePlist(v.Cdr)
			if err != nil {
				return nil, fmt.Errorf("position-map value for %q: %w", key, err)
			}
		case sexp.List:
			// List format: ("key" :k1 v1 :k2 v2)
			if len(v) < 3 {
				return nil, fmt.Errorf("position-map entry too short: %v", v)
			}
			key, err = decodeString(v[0])
			if err != nil {
				return nil, fmt.Errorf("position-map key: %w", err)
			}
			plist, err = decodePlist(sexp.List(v[1:]))
			if err != nil {
				return nil, fmt.Errorf("position-map value for %q: %w", key, err)
			}
		default:
			return nil, fmt.Errorf("position-map entries must be dotted pairs or lists, got %T", item)
		}

		idx, err := decodeInt64(plist[sexp.Symbol(":gdoc-index")])
		if err != nil {
			return nil, fmt.Errorf("position-map :gdoc-index for %q: %w", key, err)
		}

		typ, err := decodeString(plist[sexp.Symbol(":type")])
		if err != nil {
			return nil, fmt.Errorf("position-map :type for %q: %w", key, err)
		}

		out[key] = gdocs.Position{GdocIndex: idx, Type: gdocs.ContentType(typ)}
	}

	return out, nil
}

func decodeCollaboratorCache(expr sexp.Sexp) (map[string]string, error) {
	if expr == nil {
		return map[string]string{}, nil
	}
	if sym, ok := expr.(sexp.Symbol); ok && sym == "nil" {
		return map[string]string{}, nil
	}

	list, ok := expr.(sexp.List)
	if !ok {
		return nil, fmt.Errorf("expected list, got %T", expr)
	}

	out := make(map[string]string, len(list))
	for _, item := range list {
		cons, ok := item.(sexp.Cons)
		if !ok {
			return nil, fmt.Errorf("collaborator-cache entries must be dotted pairs, got %T", item)
		}
		email, err := decodeString(cons.Car)
		if err != nil {
			return nil, err
		}
		name, err := decodeString(cons.Cdr)
		if err != nil {
			return nil, err
		}
		out[email] = name
	}
	return out, nil
}

func decodeInt64(expr sexp.Sexp) (int64, error) {
	switch v := expr.(type) {
	case sexp.Number:
		return int64(v), nil
	case int:
		return int64(v), nil
	case int64:
		return v, nil
	default:
		return 0, fmt.Errorf("expected number, got %T", expr)
	}
}

func encodeComments(comments []gdocs.Comment) sexp.Sexp {
	out := make(sexp.List, 0, len(comments))
	for _, c := range comments {
		out = append(out, encodeComment(c))
	}
	return out
}

func encodeComment(c gdocs.Comment) sexp.Sexp {
	resolvedSym := sexp.Symbol("nil")
	if c.Resolved {
		resolvedSym = sexp.Symbol("t")
	}

	anchor := sexp.List{
		sexp.Symbol(":custom-id"),
		encodeOptionalString(c.Anchor.CustomID),
		sexp.Symbol(":segment-id"),
		encodeOptionalString(c.Anchor.SegmentID),
		sexp.Symbol(":start-index"),
		sexp.Number(int64(c.Anchor.StartIndex)),
		sexp.Symbol(":end-index"),
		sexp.Number(int64(c.Anchor.EndIndex)),
		sexp.Symbol(":quoted-text"),
		sexp.String(c.Anchor.QuotedText),
	}

	return sexp.List{
		sexp.Symbol("comment"),
		sexp.Symbol(":id"),
		sexp.String(c.ID),
		sexp.Symbol(":author-email"),
		sexp.String(c.AuthorEmail),
		sexp.Symbol(":author-name"),
		sexp.String(c.AuthorName),
		sexp.Symbol(":text"),
		sexp.String(c.Text),
		sexp.Symbol(":created"),
		sexp.String(c.Created),
		sexp.Symbol(":resolved"),
		resolvedSym,
		sexp.Symbol(":anchor"),
		anchor,
	}
}

func encodeOptionalString(s string) sexp.Sexp {
	if s == "" {
		return sexp.Symbol("nil")
	}
	return sexp.String(s)
}

func encodeCollaborators(m map[string]string) sexp.Sexp {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	out := make(sexp.List, 0, len(keys))
	for _, k := range keys {
		out = append(out, sexp.Cons{
			Car: sexp.String(k),
			Cdr: sexp.String(m[k]),
		})
	}
	return out
}
