package gdocs

import (
	"fmt"

	"google.golang.org/api/docs/v1"
)

func BuildTextStyleRequests(text string, baseIndex int64, ranges []FormatRange) ([]*docs.Request, error) {
	if len(ranges) == 0 {
		return nil, nil
	}

	runeOffsets := utf16Offsets(text)
	runeCount := len(runeOffsets) - 1

	reqs := make([]*docs.Request, 0, len(ranges))
	for _, r := range ranges {
		if r.Start < 0 || r.End < 0 || r.Start > r.End || r.End > runeCount {
			return nil, fmt.Errorf("format range out of bounds: %+v (text runes=%d)", r, runeCount)
		}

		start := baseIndex + runeOffsets[r.Start]
		end := baseIndex + runeOffsets[r.End]

		// Google Docs can't attach link styles to newline characters. We avoid
		// formatting the trailing newline by clamping to the inserted text.
		if end < start {
			continue
		}

		textStyle := &docs.TextStyle{}
		fields := ""

		switch r.Type {
		case FormatBold:
			textStyle.Bold = true
			fields = "bold"
		case FormatItalic:
			textStyle.Italic = true
			fields = "italic"
		case FormatUnderline:
			textStyle.Underline = true
			fields = "underline"
		case FormatStrikethrough:
			textStyle.Strikethrough = true
			fields = "strikethrough"
		case FormatCode:
			textStyle.WeightedFontFamily = &docs.WeightedFontFamily{
				FontFamily: "Courier New",
				Weight:     400,
			}
			fields = "weightedFontFamily"
		case FormatLink:
			if r.URL == "" {
				return nil, fmt.Errorf("link format range requires url: %+v", r)
			}
			textStyle.Link = &docs.Link{Url: r.URL}
			fields = "link"
		default:
			return nil, fmt.Errorf("unsupported format type: %q", r.Type)
		}

		reqs = append(reqs, &docs.Request{
			UpdateTextStyle: &docs.UpdateTextStyleRequest{
				Range: &docs.Range{
					StartIndex: start,
					EndIndex:   end,
				},
				TextStyle: textStyle,
				Fields:    fields,
			},
		})
	}

	return reqs, nil
}
