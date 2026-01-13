package gdocs

import (
	"context"
	"fmt"
	"sort"
	"strings"

	"google.golang.org/api/docs/v1"
	"org2gdocs/debug"
)

type Position struct {
	GdocIndex int64
	Type      ContentType
}

type PushResult struct {
	DocumentID  string
	DocumentURL string
	PositionMap map[string]Position
}

func PushDocument(ctx context.Context, c *Client, docID string, title string, content []Content) (*PushResult, error) {
	existingDoc := docID != ""

	if docID == "" {
		doc, err := c.Docs.Documents.Create(&docs.Document{Title: title}).Context(ctx).Do()
		if err != nil {
			return nil, err
		}
		docID = doc.DocumentId
	}

	positionMap := make(map[string]Position, len(content))

	index := int64(1) // start-of-document (UTF-16 code units)
	pending := []*docs.Request{}

	// Set reasonable document margins (1 inch = 72 points)
	marginPt := &docs.Dimension{Magnitude: 72, Unit: "PT"}
	pending = append(pending, &docs.Request{
		UpdateDocumentStyle: &docs.UpdateDocumentStyleRequest{
			DocumentStyle: &docs.DocumentStyle{
				MarginTop:    marginPt,
				MarginBottom: marginPt,
				MarginLeft:   marginPt,
				MarginRight:  marginPt,
			},
			Fields: "marginTop,marginBottom,marginLeft,marginRight",
		},
	})

	flush := func() error {
		if len(pending) == 0 {
			return nil
		}
		_, err := c.Docs.Documents.BatchUpdate(docID, &docs.BatchUpdateDocumentRequest{
			Requests: pending,
		}).Context(ctx).Do()
		pending = pending[:0]
		return err
	}

	if existingDoc {
		endIndex, err := getDocumentEndIndex(ctx, c, docID)
		if err != nil {
			return nil, err
		}

		// Keep the final newline/paragraph boundary by deleting up to endIndex-1.
		if endIndex > 1 {
			deleteEnd := endIndex - 1
			if deleteEnd > 1 {
				pending = append(pending, &docs.Request{
					DeleteContentRange: &docs.DeleteContentRangeRequest{
						Range: &docs.Range{StartIndex: 1, EndIndex: deleteEnd},
					},
				})
			}
		}
	}

	for _, item := range content {
		if item.CustomID != "" {
			positionMap[item.CustomID] = Position{
				GdocIndex: index,
				Type:      item.Type,
			}
		}

		switch item.Type {
		case ContentHeading:
			reqs, newIndex, err := buildHeadingRequests(index, item.Level, item.Text)
			if err != nil {
				return nil, err
			}
			pending = append(pending, reqs...)
			index = newIndex

		case ContentParagraph:
			reqs, newIndex, err := buildParagraphRequests(index, item.Text, item.Formatting)
			if err != nil {
				return nil, err
			}
			pending = append(pending, reqs...)
			index = newIndex

		case ContentList:
			reqs, newIndex, err := buildListRequests(index, item.ListType, item.Items)
			if err != nil {
				return nil, err
			}
			pending = append(pending, reqs...)
			index = newIndex

		case ContentTable:
			// Tables require multiple API calls to determine cell indices.
			if err := flush(); err != nil {
				return nil, err
			}

			newIndex, err := insertAndFillTable(ctx, c, docID, index, item.Rows)
			if err != nil {
				return nil, err
			}
			index = newIndex

		case ContentImage:
			if err := flush(); err != nil {
				return nil, err
			}

			newIndex, err := insertImage(ctx, c, docID, index, item.ImagePath, item.AltText)
			if err != nil {
				return nil, err
			}
			index = newIndex

		default:
			return nil, fmt.Errorf("unsupported content type: %q", item.Type)
		}
	}

	if err := flush(); err != nil {
		return nil, err
	}

	return &PushResult{
		DocumentID:  docID,
		DocumentURL: fmt.Sprintf("https://docs.google.com/document/d/%s/edit", docID),
		PositionMap: positionMap,
	}, nil
}

func getDocumentEndIndex(ctx context.Context, c *Client, docID string) (int64, error) {
	doc, err := c.Docs.Documents.Get(docID).Context(ctx).Do()
	if err != nil {
		return 0, err
	}
	return documentEndIndex(doc), nil
}

func documentEndIndex(doc *docs.Document) int64 {
	var end int64 = 1
	if doc.Body == nil {
		return end
	}
	for _, el := range doc.Body.Content {
		if el.EndIndex > end {
			end = el.EndIndex
		}
	}
	return end
}

func buildHeadingRequests(index int64, level int, text string) ([]*docs.Request, int64, error) {
	if strings.TrimSpace(text) == "" {
		text = ""
	}

	if level < 1 {
		level = 1
	}
	if level > 6 {
		level = 6
	}

	insertText := text + "\n"
	newIndex := index + utf16LenString(insertText)

	namedStyle := fmt.Sprintf("HEADING_%d", level)
	reqs := []*docs.Request{
		{
			InsertText: &docs.InsertTextRequest{
				Location: &docs.Location{Index: index},
				Text:     insertText,
			},
		},
		{
			UpdateParagraphStyle: &docs.UpdateParagraphStyleRequest{
				Range: &docs.Range{
					StartIndex: index,
					EndIndex:   newIndex,
				},
				ParagraphStyle: &docs.ParagraphStyle{NamedStyleType: namedStyle},
				Fields:         "namedStyleType",
			},
		},
	}

	return reqs, newIndex, nil
}

func buildParagraphRequests(index int64, text string, formatting []FormatRange) ([]*docs.Request, int64, error) {
	insertText := text + "\n"
	newIndex := index + utf16LenString(insertText)

	reqs := []*docs.Request{
		{
			InsertText: &docs.InsertTextRequest{
				Location: &docs.Location{Index: index},
				Text:     insertText,
			},
		},
	}

	styleReqs, err := BuildTextStyleRequests(text, index, formatting)
	if err != nil {
		return nil, 0, err
	}
	reqs = append(reqs, styleReqs...)

	return reqs, newIndex, nil
}

func buildListRequests(index int64, listType ListType, items []string) ([]*docs.Request, int64, error) {
	if len(items) == 0 {
		return nil, index, nil
	}

	joined := strings.Join(items, "\n") + "\n"
	newIndex := index + utf16LenString(joined)

	var preset string
	switch listType {
	case ListOrdered:
		preset = "NUMBERED_DECIMAL_ALPHA_ROMAN"
	default:
		preset = "BULLET_DISC_CIRCLE_SQUARE"
	}

	reqs := []*docs.Request{
		{
			InsertText: &docs.InsertTextRequest{
				Location: &docs.Location{Index: index},
				Text:     joined,
			},
		},
		{
			CreateParagraphBullets: &docs.CreateParagraphBulletsRequest{
				Range: &docs.Range{
					StartIndex: index,
					EndIndex:   newIndex,
				},
				BulletPreset: preset,
			},
		},
	}

	return reqs, newIndex, nil
}

func insertAndFillTable(ctx context.Context, c *Client, docID string, index int64, rows [][]string) (int64, error) {
	if len(rows) == 0 {
		return index, nil
	}

	cols := 0
	for _, r := range rows {
		if len(r) > cols {
			cols = len(r)
		}
	}
	if cols == 0 {
		return index, nil
	}

	debug.Log("Table insertion: %d rows x %d cols", len(rows), cols)
	for i, row := range rows {
		debug.Log("  Row %d: %v", i, row)
	}

	// Step 0: Fetch the document to get the actual current end index.
	// This handles index drift from UTF-16 calculations or structural elements.
	doc, err := c.Docs.Documents.Get(docID).Context(ctx).Do()
	if err != nil {
		return 0, err
	}
	actualEndIndex := documentEndIndex(doc) - 1
	if actualEndIndex < 1 {
		actualEndIndex = 1
	}

	// Use the actual end index for table insertion
	insertIndex := actualEndIndex

	// Step 1: Insert the table skeleton.
	_, err = c.Docs.Documents.BatchUpdate(docID, &docs.BatchUpdateDocumentRequest{
		Requests: []*docs.Request{
			{
				InsertTable: &docs.InsertTableRequest{
					Rows:    int64(len(rows)),
					Columns: int64(cols),
					Location: &docs.Location{
						Index: insertIndex,
					},
				},
			},
		},
	}).Context(ctx).Do()
	if err != nil {
		return 0, err
	}

	// Step 2: Fetch the doc to find the new table and compute cell insertion
	// indices.
	doc, err = c.Docs.Documents.Get(docID).Context(ctx).Do()
	if err != nil {
		return 0, err
	}

	tableEl := findTableElement(doc, insertIndex)
	if tableEl == nil || tableEl.Table == nil {
		return 0, fmt.Errorf("inserted table not found at index %d (expected %d)", insertIndex, index)
	}

	// Step 3: Insert cell contents.
	debug.Log("Table found: %d rows in doc", len(tableEl.Table.TableRows))
	cellReqs := []*docs.Request{}
	for r, row := range rows {
		if r >= len(tableEl.Table.TableRows) {
			debug.Log("  Row %d: skipping (out of bounds)", r)
			break
		}
		tableRow := tableEl.Table.TableRows[r]
		debug.Log("  Row %d: %d cells in doc row", r, len(tableRow.TableCells))
		for cidx, cellText := range row {
			if cidx >= len(tableRow.TableCells) {
				debug.Log("    Cell [%d][%d]: skipping (out of bounds)", r, cidx)
				break
			}
			cell := tableRow.TableCells[cidx]
			cellStart := cellStartIndex(cell)
			debug.Log("    Cell [%d][%d]: start=%d text=%q", r, cidx, cellStart, cellText)
			if cellStart == 0 {
				continue
			}
			if strings.TrimSpace(cellText) == "" {
				continue
			}
			cellReqs = append(cellReqs, &docs.Request{
				InsertText: &docs.InsertTextRequest{
					Location: &docs.Location{Index: cellStart},
					Text:     cellText,
				},
			})
		}
	}

	if len(cellReqs) > 0 {
		// Sort requests in reverse order by index (highest first).
		// When batched, insertions at higher indices must come first
		// so they don't shift when earlier text is inserted.
		sort.Slice(cellReqs, func(i, j int) bool {
			return cellReqs[i].InsertText.Location.Index > cellReqs[j].InsertText.Location.Index
		})
		debug.Log("Sorted %d cell requests by index (descending)", len(cellReqs))

		if _, err := c.Docs.Documents.BatchUpdate(docID, &docs.BatchUpdateDocumentRequest{Requests: cellReqs}).Context(ctx).Do(); err != nil {
			return 0, err
		}
	}

	// Step 4: Fetch doc again to get updated end index after cell insertion.
	doc, err = c.Docs.Documents.Get(docID).Context(ctx).Do()
	if err != nil {
		return 0, err
	}

	tableEl = findTableElement(doc, index)
	if tableEl == nil {
		return 0, fmt.Errorf("table not found after filling at index %d", index)
	}

	return tableEl.EndIndex, nil
}

func findTableElement(doc *docs.Document, startIndex int64) *docs.StructuralElement {
	if doc.Body == nil {
		return nil
	}

	// First try exact match
	for _, el := range doc.Body.Content {
		if el.Table != nil && el.StartIndex == startIndex {
			return el
		}
	}

	// If no exact match, find the table nearest to (but not before) startIndex.
	// This handles index drift from newlines and structural elements.
	var best *docs.StructuralElement
	var bestDist int64 = -1
	for _, el := range doc.Body.Content {
		if el.Table == nil {
			continue
		}
		dist := el.StartIndex - startIndex
		if dist >= 0 && (bestDist < 0 || dist < bestDist) {
			best = el
			bestDist = dist
		}
	}

	// Also consider tables slightly before startIndex (within 5 indices)
	// to handle cases where preceding content was shorter than expected
	if best == nil {
		for _, el := range doc.Body.Content {
			if el.Table == nil {
				continue
			}
			dist := startIndex - el.StartIndex
			if dist >= 0 && dist <= 5 && (bestDist < 0 || dist < bestDist) {
				best = el
				bestDist = dist
			}
		}
	}

	return best
}

func cellStartIndex(cell *docs.TableCell) int64 {
	if cell == nil {
		return 0
	}
	if len(cell.Content) == 0 {
		return 0
	}
	// Insert before the cell's newline in the first paragraph.
	return cell.Content[0].StartIndex
}
