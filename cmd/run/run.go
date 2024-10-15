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
		resultFmt := strconv.FormatFloat(result, 'f', -1, 64)
		fmt.Printf("=> %s\n", resultFmt)
	}
}
