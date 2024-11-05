package parser_test

import (
	"github.com/pedrothome1/goscript/internal/parser"
	"os"
	"path/filepath"
	"testing"
)

func TestParser(t *testing.T) {
	matches, err := filepath.Glob(filepath.Join("testdata", "*.gs"))
	if err != nil {
		t.Fatalf("filepath.Glob: %s", err.Error())
	}
	if len(matches) == 0 {
		t.Fatal("testdata/*.gs files not found")
	}

	for _, path := range matches {
		t.Run(path, func(t *testing.T) {
			src, err := os.ReadFile(path)
			if err != nil {
				t.Errorf("os.ReadFile: %s", err.Error())
			}

			var p parser.Parser
			err = p.Init(string(src))
			if err != nil {
				t.Errorf("Init error on %s: %s", path, err.Error())
			}

			_, err = p.Parse()
			if err != nil {
				t.Errorf("Parse error on %s: %s", path, err.Error())
			}
		})
	}
}
