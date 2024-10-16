package main

import (
	"bufio"
	"fmt"
	"github.com/pedrothome1/goscript/internal/interpreter"
	"github.com/pedrothome1/goscript/internal/parser"
	"log"
	"os"
	"strconv"
)

func main() {
	log.SetFlags(0)
	eval := &interpreter.Interpreter{}
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
		expr, err := p.Parse()
		if err != nil {
			log.Printf("parse error: %s\n", err.Error())
			continue
		}
		result, err := eval.Run(expr)
		if err != nil {
			log.Printf("interpreter error: %s\n", err.Error())
			continue
		}

		switch v := result.(type) {
		case int:
			fmt.Printf("%d\n", v)
		case bool:
			fmt.Printf("%v\n", v)
		case float64:
			fmt.Printf("%s\n", strconv.FormatFloat(v, 'f', -1, 64))
		case string:
			fmt.Printf("%q\n", v)
		case any:
			if v == nil {
				fmt.Printf("%v\n", v)
			}
		}
	}
}
