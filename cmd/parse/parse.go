package main

import (
	"bufio"
	"errors"
	"fmt"
	"github.com/pedrothome1/goscript/internal/parser"
	"github.com/pedrothome1/goscript/internal/printer"
	"github.com/pedrothome1/goscript/internal/scanner"
	"log"
	"os"
	"strings"
)

func main() {
	log.SetFlags(0)

	if len(os.Args) > 2 {
		log.Fatal("Usage: parse <file>")
	}

	if len(os.Args) == 1 {
		repl()
		return
	}

	writeAnswer(os.Args[1])
}

func writeAnswer(path string) {
	b, err := os.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}

	psr := &parser.Parser{}
	psr.Init(string(b))

	prog, err := psr.Parse()
	if err != nil {
		log.Fatal(err)
	}

	pr := &printer.Printer{}

	ansPath, _ := strings.CutSuffix(path, ".gs")
	ansPath += ".ans"

	err = os.WriteFile(ansPath, []byte(pr.StmtsString(prog)), 0666)
	if err != nil {
		log.Fatal(err)
	}
}

func repl() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanLines)

	pr := &printer.Printer{}

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
		err := p.Init(text)
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
			log.Println(err.Error())
			continue
		}

		fmt.Print(pr.StmtsString(stmts))
	}
}
