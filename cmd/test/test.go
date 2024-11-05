package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"github.com/pedrothome1/goscript/internal/parser"
	"github.com/pedrothome1/goscript/internal/printer"
	"io/fs"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

var (
	parserFlag = flag.Bool("parser", true, "run parser tests and have highest precedence")
	runFlag    = flag.Bool("run", false, "run interpreter tests")
)

func Usage() {
	fmt.Fprint(os.Stderr, "Usage:\n")
	flag.PrintDefaults()
}

func main() {
	log.SetFlags(0)
	flag.Usage = Usage
	flag.Parse()
	args := flag.Args()

	if len(args) != 1 {
		flag.Usage()
		os.Exit(1)
	}

	path := args[0]

	fi, err := os.Stat(path)
	if err != nil {
		log.Fatalf("error reading info of %q: %s", path, err.Error())
	}

	if !fi.IsDir() {
		if err = runTest(path); err != nil {
			log.Fatal(err.Error())
		}
		return
	}

	entries, err := os.ReadDir(path)
	if err != nil {
		log.Fatalf("error reading directory %q: %s", path, err.Error())
	}

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".gs") {
			continue
		}

		if err = runTest(filepath.Join(path, e.Name())); err != nil {
			log.Fatal(err.Error())
		}
	}
}

func runTest(path string) error {
	if !strings.HasSuffix(path, ".gs") {
		log.Fatalf("%s should have the extension .gs", path)
	}
	ansPath, err := checkScript(path)
	if errors.Is(err, errNoAnswer) {
		log.Printf("skipping %q: %s", path, err.Error())
		return nil
	} else if err != nil {
		log.Fatalf("error validating script %q: %s", path, err.Error())
	}

	if err = checkAnswer(path, ansPath); err != nil {
		log.Printf("FAIL %q: %s", path, err.Error())
		return nil
	}

	log.Printf("PASS %q", path)
	return nil
}

var (
	errNoAnswer = errors.New("no associated answer for script")
)

// checkScript assumes file exists and have the .gs extension.
func checkScript(path string) (string, error) {
	toks := strings.SplitN(path, ".", 2)
	ans := toks[0] + ".ans"

	_, err := os.Stat(ans)
	if errors.Is(err, fs.ErrNotExist) {
		return "", errNoAnswer
	}
	if err != nil {
		return "", err
	}

	return ans, nil
}

var (
	errFailedTest          = errors.New("output and answer don't match")
	errTestModeUnspecified = errors.New("test mode was not specified")
)

// checkAnswer assumes the files exist and are valid.
func checkAnswer(scriptPath, answerPath string) error {
	var out []byte
	var err error

	if *parserFlag {
		out, err = runParser(scriptPath)
	} else if *runFlag {
		out, err = runInterpreter(scriptPath)
	} else {
		return errTestModeUnspecified
	}
	if err != nil {
		return err
	}

	ans, err := os.ReadFile(answerPath)
	if err != nil {
		return fmt.Errorf("error reading answer %q: %s", answerPath, err.Error())
	}

	if !bytes.Equal(out, ans) {
		return errFailedTest
	}

	return nil
}

func runParser(scriptPath string) ([]byte, error) {
	b, err := os.ReadFile(scriptPath)
	if err != nil {
		return nil, err
	}

	p := &parser.Parser{}
	p.Init(string(b))

	stmts, err := p.Parse()
	if err != nil {
		return nil, err
	}

	pr := &printer.Printer{}

	return []byte(pr.StmtsString(stmts)), nil
}

func runInterpreter(scriptPath string) ([]byte, error) {
	runner, err := filepath.Abs(filepath.Join(".", "run.exe"))
	if err != nil {
		return nil, err
	}

	if _, err := os.Stat(runner); err != nil {
		return nil, fmt.Errorf("error checking runner %q: %s", runner, err.Error())
	}

	cmd := exec.Command(runner, scriptPath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("error running script %q: %s", scriptPath, err.Error())
	}

	return out, nil
}
