package main

import (
	"bufio"
	"fmt"
	"github.com/pedrothome1/goscript/internal/interpreter"
	"github.com/pedrothome1/goscript/internal/parser"
	"log"
	"os"
)

func main() {
	log.SetFlags(0)
	runner := interpreter.New()
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
			log.Printf("scan error: %s\n", err.Error())
			continue
		}
		stmts, err := p.Parse()
		if err != nil {
			log.Printf("parse error: %s\n", err.Error())
			continue
		}
		for _, stmt := range stmts {
			err = runner.Run(stmt)
			if err != nil {
				log.Printf("runtime error: %s\n", err.Error())
				continue
			}
		}
	}
}
