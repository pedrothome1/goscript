package interpreter

import "errors"

var (
	errBreak    = errors.New("break")
	errContinue = errors.New("continue")
)
