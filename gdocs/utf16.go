package gdocs

import "unicode/utf16"

func utf16LenString(s string) int64 {
	return int64(len(utf16.Encode([]rune(s))))
}

// utf16Offsets returns a prefix sum array where offsets[i] is the UTF-16 code-unit
// length of the first i runes in s.
func utf16Offsets(s string) []int64 {
	rs := []rune(s)
	offsets := make([]int64, len(rs)+1)
	var n int64
	for i, r := range rs {
		n += int64(len(utf16.Encode([]rune{r})))
		offsets[i+1] = n
	}
	return offsets
}
