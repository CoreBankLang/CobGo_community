package decimal

import "errors"

var (
	// ErrRoundingProhibited indicates rounding was attempted with PROHIBITED mode
	ErrRoundingProhibited = errors.New("rounding prohibited: exact result required")

	// ErrInvalidRoundingMode indicates an invalid rounding mode was specified
	ErrInvalidRoundingMode = errors.New("invalid rounding mode")

	// ErrDivisionByZero indicates division by zero
	ErrDivisionByZero = errors.New("division by zero")

	// ErrSizeError indicates a size error condition
	ErrSizeError = errors.New("size error: result exceeds field capacity")

	// ErrPrecisionLoss indicates potential precision loss
	ErrPrecisionLoss = errors.New("precision loss detected")
)
