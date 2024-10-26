package resolver

import "fmt"

type ResolveError struct {
	msg string
}

func (re *ResolveError) Error() string {
	return re.msg
}

func resolveErrorf(format string, args ...any) *ResolveError {
	return &ResolveError{msg: fmt.Errorf(format, args...).Error()}
}
