package scanner

import "fmt"

type ScanError struct {
	message string
	details string
}

func (e *ScanError) Error() string {
	return e.message
}

func (e *ScanError) Details() string {
	return e.details
}

func (e *ScanError) Report() {
	fmt.Print(e.details)
}
