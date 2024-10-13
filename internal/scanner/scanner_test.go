package scanner_test

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/scanner"
	"testing"
)

func TestScanner_Scan(t *testing.T) {
	src := "(3 + 4) * 5"
	s := &scanner.Scanner{}
	s.Init([]byte(src))

	toks, err := s.Scan()
	if err != nil {
		t.Errorf("err == nil: %s", err.Error())
	}

	for _, tok := range toks {
		fmt.Printf("%s ", tok.String())
	}
	fmt.Println()
}
