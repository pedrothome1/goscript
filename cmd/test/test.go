package main

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func main() {
	log.SetFlags(0)

	if len(os.Args) != 2 {
		log.Fatalf("Usage: %s <directory or file>", os.Args[0])
	}

	path := os.Args[1]

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

var errFailedTest = errors.New("output and answer don't match")

// checkAnswer assumes the files exist and are valid.
func checkAnswer(scriptPath string, answerPath string) error {
	runner, err := filepath.Abs(filepath.Join(".", "run.exe"))
	if err != nil {
		return err
	}

	if _, err := os.Stat(runner); err != nil {
		return fmt.Errorf("error checking runner %q: %s", runner, err.Error())
	}

	cmd := exec.Command(runner, scriptPath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("error running script %q: %s", scriptPath, err.Error())
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
