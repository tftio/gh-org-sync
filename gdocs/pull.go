package gdocs

import (
	"context"
	"sort"

	"org2gdocs/debug"

	"google.golang.org/api/drive/v3"
)

type PullResult struct {
	DocumentID       string
	Comments         []Comment
	SuggestedEdits   []any
	NewCollaborators map[string]string
}

type PullRequest struct {
	DocumentID        string
	PositionMap       map[string]Position
	KnownCommentIDs   map[string]struct{}
	CollaboratorCache map[string]string
}

func Pull(ctx context.Context, c *Client, req PullRequest) (*PullResult, error) {
	posEntries := sortedPositions(req.PositionMap)

	newCollaborators := make(map[string]string)
	comments := []Comment{}

	call := c.Drive.Comments.List(req.DocumentID).
		IncludeDeleted(false).
		PageSize(100).
		Fields(
			"comments(anchor,author(displayName,emailAddress),content,createdTime,deleted,id,resolved,quotedFileContent(value),replies(action,author(displayName,emailAddress),content,createdTime,deleted,id)),nextPageToken",
		)

	err := call.Pages(ctx, func(list *drive.CommentList) error {
		for _, dc := range list.Comments {
			if dc == nil || dc.Deleted {
				continue
			}

			collectCollaboratorsFromDriveComment(dc, req.CollaboratorCache, newCollaborators)

			if _, known := req.KnownCommentIDs[dc.Id]; known {
				continue
			}

			authorEmail := ""
			authorName := ""
			if dc.Author != nil {
				authorEmail = dc.Author.EmailAddress
				authorName = dc.Author.DisplayName
			}
			if authorName == "" {
				if cached, ok := req.CollaboratorCache[authorEmail]; ok {
					authorName = cached
				} else {
					authorName = ExtractDisplayName(authorEmail)
				}
			}

			quoted := ""
			if dc.QuotedFileContent != nil {
				quoted = dc.QuotedFileContent.Value
			}

			anchor := mapAnchor(dc.Anchor, quoted, posEntries)

			comments = append(comments, Comment{
				ID:          dc.Id,
				AuthorEmail: authorEmail,
				AuthorName:  authorName,
				Text:        dc.Content,
				Created:     dc.CreatedTime,
				Resolved:    dc.Resolved,
				Anchor:      anchor,
			})
		}
		return nil
	})
	if err != nil {
		return nil, err
	}

	sort.Slice(comments, func(i, j int) bool { return comments[i].Created < comments[j].Created })

	debug.Log("Pull: fetched %d comment(s) (%d new)", len(comments)+len(req.KnownCommentIDs), len(comments))

	return &PullResult{
		DocumentID:       req.DocumentID,
		Comments:         comments,
		SuggestedEdits:   nil,
		NewCollaborators: newCollaborators,
	}, nil
}

func collectCollaboratorsFromDriveComment(dc *drive.Comment, known map[string]string, out map[string]string) {
	addUser := func(u *drive.User) {
		if u == nil {
			return
		}
		email := u.EmailAddress
		if email == "" {
			return
		}
		if _, ok := known[email]; ok {
			return
		}
		if _, ok := out[email]; ok {
			return
		}
		name := u.DisplayName
		if name == "" {
			name = ExtractDisplayName(email)
		}
		if name == "" {
			return
		}
		out[email] = name
	}

	addUser(dc.Author)
	for _, r := range dc.Replies {
		if r == nil || r.Deleted {
			continue
		}
		addUser(r.Author)
	}
}
