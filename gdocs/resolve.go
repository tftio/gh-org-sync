package gdocs

import (
	"context"

	"org2gdocs/debug"

	"google.golang.org/api/drive/v3"
)

// ResolveComments marks the specified comment IDs as resolved in Google Docs.
// It returns the list of successfully resolved comment IDs and any errors
// encountered. Errors for individual comments do not stop processing of others.
func ResolveComments(ctx context.Context, c *Client, docID string, commentIDs []string) ([]string, error) {
	if len(commentIDs) == 0 {
		return nil, nil
	}

	debug.Log("Resolving %d comment(s) in document %s", len(commentIDs), docID)

	resolved := make([]string, 0, len(commentIDs))

	for _, commentID := range commentIDs {
		debug.Log("Resolving comment: %s", commentID)

		_, err := c.Drive.Comments.Update(docID, commentID, &drive.Comment{
			Resolved: true,
		}).Fields("id,resolved").Context(ctx).Do()

		if err != nil {
			debug.Log("Failed to resolve comment %s: %v", commentID, err)
			// Continue with other comments rather than failing completely.
			continue
		}

		debug.Log("Successfully resolved comment: %s", commentID)
		resolved = append(resolved, commentID)
	}

	debug.Log("Resolved %d/%d comment(s)", len(resolved), len(commentIDs))

	return resolved, nil
}
