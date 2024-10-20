package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"github.com/pedrothome1/goscript/internal/interpreter"
	"github.com/pedrothome1/goscript/internal/parser"
	"github.com/pedrothome1/goscript/internal/scanner"
	"log"
	"os"
)

func main() {
	log.SetFlags(0)
	flag.Parse()

	args := flag.Args()
	if len(args) > 1 {
		log.Fatal("too many arguments")
	}

	runner := interpreter.New()
	if len(args) == 1 {
		src, err := os.ReadFile(args[0])
		if err != nil {
			log.Fatalf("error reading file %s: %s", args[0], err.Error())
		}
		p := &parser.Parser{}
		err = p.Init(src)
		if err != nil {
			var scanErr *scanner.ScanError
			if errors.As(err, &scanErr) {
				log.Fatal(scanErr.Details())
			}
			log.Fatalf("scan error: %s\n", err.Error())
		}
		stmts, err := p.Parse()
		if err != nil {
			log.Fatalf("parse error: %s\n", err.Error())
		}
		err = runner.RunProgram(stmts)
		if err != nil {
			log.Fatalf("runtime error: %s\n", err.Error())
		}
		return
	}

	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanLines)

	for {
		fmt.Print(">> ")
		if !sc.Scan() {
			break
		}
		text := sc.Text()
		if text == "q" {
			break
		}
		p := &parser.Parser{}
		err := p.Init([]byte(text))
		if err != nil {
			var scanErr *scanner.ScanError
			if errors.As(err, &scanErr) {
				log.Print(scanErr.Details())
			} else {
				log.Printf("scan error: %s\n", err.Error())
			}
			continue
		}
		stmts, err := p.Parse()
		if err != nil {
			log.Printf("parse error: %s\n", err.Error())
			continue
		}
		err = runner.RunProgram(stmts)
		if err != nil {
			log.Printf("runtime error: %s\n", err.Error())
			continue
		}
	}
}
