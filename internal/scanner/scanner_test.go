package scanner_test

import (
	"errors"
	"fmt"
	"github.com/pedrothome1/goscript/internal/scanner"
	"testing"
)

func TestScanner_Scan(t *testing.T) {
	// print tokens
	src := "(3 + 4) * 5"
	s := &scanner.Scanner{}
	s.Init([]byte(src))

	toks, err := s.Scan()
	if err != nil {
		t.Errorf("err == nil: %s\n", err.Error())
	}

	for _, tok := range toks {
		fmt.Printf("%s ", tok.String())
	}
	fmt.Println()
}

func TestScanner_Scan_ErrorPosition(t *testing.T) {
	// print correct position of offending token
	src := `var a = 1
var b = 1 + 2
var b $= 2
`

	s := &scanner.Scanner{}
	s.Init([]byte(src))

	var scanErr *scanner.ScanError
	_, err := s.Scan()
	if errors.As(err, &scanErr) {
		scanErr.Report()
	}
}
