package main

import (
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2/helpers"
)

var regexp2cgBenchSinkInt int
var regexp2cgBenchSinkBool bool

func BenchmarkRegexp2CGIndexASCIIIgnoreCase(b *testing.B) {
	input := strings.Repeat("0123456789abcdefghijklmnopqrstuvwxyz_", 256) + "TeStToken"
	prefix := "testtoken"
	b.ReportAllocs()
	b.SetBytes(int64(len(input)))

	for i := 0; i < b.N; i++ {
		regexp2cgBenchSinkInt = helpers.IndexStringIgnoreCaseASCII(input, prefix)
	}
}

func BenchmarkRegexp2CGASCIIBitmapContains(b *testing.B) {
	input := []rune(strings.Repeat("abcXYZ019_-:/", 1024))
	var lo, hi uint64
	for _, ch := range []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") {
		if ch < 64 {
			lo |= 1 << uint(ch)
		} else {
			hi |= 1 << uint(ch-64)
		}
	}

	b.ReportAllocs()
	b.SetBytes(int64(len(input)))

	for i := 0; i < b.N; i++ {
		found := false
		for _, ch := range input {
			found = regexp2cgBenchmarkASCIIWordClass(ch, lo, hi)
		}
		regexp2cgBenchSinkBool = found
	}
}

func regexp2cgBenchmarkASCIIWordClass(ch rune, lo, hi uint64) bool {
	return helpers.IsInASCIIBitmap(ch, lo, hi)
}
