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
)

func main() {
	log.SetFlags(0)
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
			log.Println(err.Error())
			continue
		}

		for _, stmt := range stmts {
			fmt.Print(pr.StmtString(stmt))
			fmt.Println()
		}
	}
}
