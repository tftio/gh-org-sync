package gdocs

type ContentType string

const (
	ContentHeading   ContentType = "heading"
	ContentParagraph ContentType = "paragraph"
	ContentList      ContentType = "list"
	ContentTable     ContentType = "table"
	ContentImage     ContentType = "image"
)

type ListType string

const (
	ListUnordered ListType = "unordered"
	ListOrdered   ListType = "ordered"
)

type FormatType string

const (
	FormatBold          FormatType = "bold"
	FormatItalic        FormatType = "italic"
	FormatUnderline     FormatType = "underline"
	FormatStrikethrough FormatType = "strikethrough"
	FormatCode          FormatType = "code"
	FormatLink          FormatType = "link"
)

type Content struct {
	Type     ContentType
	Level    int
	Text     string
	CustomID string

	Formatting []FormatRange

	// Lists
	ListType ListType
	Items    []string

	// Tables
	Rows [][]string

	// Images (Task 3)
	ImagePath string
	AltText   string
}

type FormatRange struct {
	Type  FormatType
	Start int
	End   int
	URL   string
}
