package main

import (
	"bufio"
	"fmt"
	"github.com/pedrothome1/goscript/internal/parser"
	"github.com/pedrothome1/goscript/internal/printer"
	"log"
	"os"
)

func main() {
	log.SetFlags(0)
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
		p.Init([]byte(text))

		expr, err := p.Parse()
		if err != nil {
			log.Println(err.Error())
			continue
		}

		pr := &printer.Printer{}
		fmt.Print(pr.String(expr))
	}
}
