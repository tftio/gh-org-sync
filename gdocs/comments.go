package gdocs

import (
	"encoding/json"
	"sort"
	"strings"
	"unicode"
)

type Comment struct {
	ID          string
	AuthorEmail string
	AuthorName  string
	Text        string
	Created     string
	Resolved    bool
	Anchor      CommentAnchor
}

type CommentAnchor struct {
	CustomID   string
	SegmentID  string
	StartIndex int
	EndIndex   int
	QuotedText string
}

func ExtractDisplayName(email string) string {
	// Simple heuristic: jane.smith@company.com -> Jane Smith
	if email == "" {
		return ""
	}

	local := strings.SplitN(email, "@", 2)[0]
	parts := strings.FieldsFunc(local, func(r rune) bool {
		return r == '.' || r == '-' || r == '_' || r == '+'
	})

	out := make([]string, 0, len(parts))
	for _, part := range parts {
		if part == "" {
			continue
		}
		out = append(out, titleWord(part))
	}
	return strings.Join(out, " ")
}

func titleWord(s string) string {
	runes := []rune(strings.ToLower(s))
	if len(runes) == 0 {
		return ""
	}
	runes[0] = unicode.ToUpper(runes[0])
	return string(runes)
}

type posEntry struct {
	ID    string
	Start int64
	Type  ContentType
}

func sortedPositions(posMap map[string]Position) []posEntry {
	out := make([]posEntry, 0, len(posMap))
	for id, pos := range posMap {
		out = append(out, posEntry{ID: id, Start: pos.GdocIndex, Type: pos.Type})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Start < out[j].Start })
	return out
}

func mapAnchor(anchorJSON string, quotedText string, positions []posEntry) CommentAnchor {
	if strings.TrimSpace(anchorJSON) == "" {
		return CommentAnchor{
			CustomID:   "",
			SegmentID:  "",
			StartIndex: 0,
			EndIndex:   0,
			QuotedText: quotedText,
		}
	}

	start, end, ok := parseAnchorStartEnd(anchorJSON)
	if !ok {
		return CommentAnchor{
			CustomID:   "",
			SegmentID:  "",
			StartIndex: 0,
			EndIndex:   0,
			QuotedText: quotedText,
		}
	}

	segID, segStart := mapIndexToSegment(start, positions)
	customID := segID
	if i := strings.Index(segID, "/"); i >= 0 {
		customID = segID[:i]
	}

	relStart := int(start - segStart)
	relEnd := int(end - segStart)
	if relStart < 0 {
		relStart = 0
	}
	if relEnd < relStart {
		relEnd = relStart
	}

	return CommentAnchor{
		CustomID:   customID,
		SegmentID:  segID,
		StartIndex: relStart,
		EndIndex:   relEnd,
		QuotedText: quotedText,
	}
}

func mapIndexToSegment(index int64, positions []posEntry) (segmentID string, segmentStart int64) {
	if len(positions) == 0 {
		return "", 0
	}

	// Find the last position whose start is <= index.
	i := sort.Search(len(positions), func(i int) bool { return positions[i].Start > index }) - 1
	if i < 0 {
		return "", 0
	}

	return positions[i].ID, positions[i].Start
}

func parseAnchorStartEnd(anchorJSON string) (start int64, end int64, ok bool) {
	var v any
	dec := json.NewDecoder(strings.NewReader(anchorJSON))
	dec.UseNumber()
	if err := dec.Decode(&v); err != nil {
		return 0, 0, false
	}

	start, end, ok = findStartEndInJSON(v)
	if !ok {
		return 0, 0, false
	}
	if end < start {
		end = start
	}
	return start, end, true
}

func findStartEndInJSON(v any) (start int64, end int64, ok bool) {
	switch t := v.(type) {
	case map[string]any:
		// Prefer maps that contain both keys.
		if s, okS := getJSONIntForKeys(t, []string{"startIndex", "start", "s"}); okS {
			if e, okE := getJSONIntForKeys(t, []string{"endIndex", "end", "e"}); okE {
				return s, e, true
			}
		}

		for _, vv := range t {
			if s, e, ok := findStartEndInJSON(vv); ok {
				return s, e, ok
			}
		}
	case []any:
		for _, vv := range t {
			if s, e, ok := findStartEndInJSON(vv); ok {
				return s, e, ok
			}
		}
	}

	return 0, 0, false
}

func getJSONIntForKeys(m map[string]any, keys []string) (int64, bool) {
	for _, k := range keys {
		v, ok := m[k]
		if !ok {
			continue
		}

		switch n := v.(type) {
		case json.Number:
			i, err := n.Int64()
			if err != nil {
				continue
			}
			return i, true
		case float64:
			return int64(n), true
		case int64:
			return n, true
		case int:
			return int64(n), true
		}
	}
	return 0, false
}
