package main

import (
	"bufio"
	"errors"
	"fmt"
	"github.com/pedrothome1/goscript/internal/scanner"
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
		s := &scanner.Scanner{}
		s.Init([]byte(text))
		toks, err := s.Scan()
		if err != nil {
			var scanError *scanner.ScanError
			if errors.As(err, &scanError) {
				scanError.Report()
			} else {
				log.Println(err.Error())
			}
			continue
		}
		for _, t := range toks {
			fmt.Printf("%s\n", t.String())
		}
		fmt.Println()
	}
}
