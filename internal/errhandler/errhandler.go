package errhandler

import "fmt"

func Err(kind, message string, line, col uint) error {
	return fmt.Errorf("%s error at line %d, column %d: %s", kind, line, col, message)
}
