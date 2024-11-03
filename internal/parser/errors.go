package parser

import (
	"fmt"
	"runtime"
	"slices"
	"strings"
)

const debug = true

func parseErrorf(format string, args ...any) error {
	if debug {
		const bufSize = 30
		excludeFuncs := []string{"goexit", "main", "Parse"}
		pcs := make([]uintptr, bufSize)
		written := runtime.Callers(2, pcs)
		if written == bufSize {
			fmt.Println("there were more function calls than expected")
		}
		frames := runtime.CallersFrames(pcs)
		for {
			f, more := frames.Next()
			toks := strings.Split(f.Function, ".")
			fn := toks[len(toks)-1]
			if !slices.Contains(excludeFuncs, fn) {
				fmt.Printf("%s:%d\n", fn, f.Line)
			}

			if !more {
				break
			}
		}
		fmt.Println()
	}

	return fmt.Errorf(format, args...)
}
