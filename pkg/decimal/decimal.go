package decimal

import (
	"fmt"
	"math/big"
	"strings"
	"sync"
)

// Pool for reusing big.Int objects to reduce allocations
var bigIntPool = sync.Pool{
	New: func() interface{} {
		return new(big.Int)
	},
}

// getBigInt gets a big.Int from the pool
func getBigInt() *big.Int {
	return bigIntPool.Get().(*big.Int)
}

// putBigInt returns a big.Int to the pool
func putBigInt(bi *big.Int) {
	bi.SetInt64(0) // Reset to zero
	bigIntPool.Put(bi)
}

// Decimal represents a fixed-point decimal number with arbitrary precision
type Decimal struct {
	value     *big.Int
	scale     int32
	precision int32
}

// NewFromString creates a new Decimal from a string representation
func NewFromString(s string) (*Decimal, error) {
	// Remove any whitespace
	s = strings.TrimSpace(s)

	// Handle empty string
	if s == "" {
		return &Decimal{
			value:     big.NewInt(0),
			scale:     0,
			precision: 1,
		}, nil
	}

	// Check for valid decimal format
	if !isValidDecimalString(s) {
		return nil, fmt.Errorf("invalid decimal string: %s", s)
	}

	// Split into integer and fractional parts
	parts := strings.Split(s, ".")
	var integerPart, fractionalPart string

	if len(parts) == 1 {
		integerPart = parts[0]
		fractionalPart = ""
	} else if len(parts) == 2 {
		integerPart = parts[0]
		fractionalPart = parts[1]
	} else {
		return nil, fmt.Errorf("invalid decimal format: %s", s)
	}

	// Handle negative numbers
	negative := false
	if strings.HasPrefix(integerPart, "-") {
		negative = true
		integerPart = integerPart[1:]
	}

	// Remove leading zeros from integer part
	integerPart = strings.TrimLeft(integerPart, "0")
	if integerPart == "" {
		integerPart = "0"
	}

	// Remove trailing zeros from fractional part
	fractionalPart = strings.TrimRight(fractionalPart, "0")

	// Calculate scale (number of decimal places)
	scale := int32(len(fractionalPart))

	// Combine integer and fractional parts
	combined := integerPart + fractionalPart
	if combined == "0" {
		scale = 0
	}

	// Parse as big.Int
	value, ok := new(big.Int).SetString(combined, 10)
	if !ok {
		return nil, fmt.Errorf("invalid decimal value: %s", combined)
	}

	// Apply negative sign
	if negative {
		value.Neg(value)
	}

	// Calculate precision (total number of significant digits)
	precision := int32(len(strings.TrimLeft(strings.TrimRight(combined, "0"), "0")))
	if precision == 0 {
		precision = 1
	}

	return &Decimal{
		value:     value,
		scale:     scale,
		precision: precision,
	}, nil
}

// NewFromInt64 creates a new Decimal from an int64
func NewFromInt64(i int64) *Decimal {
	return &Decimal{
		value:     big.NewInt(i),
		scale:     0,
		precision: countDigits(i),
	}
}

// NewFromFloat64 creates a new Decimal from a float64
func NewFromFloat64(f float64) (*Decimal, error) {
	// Convert to string with enough precision, but remove trailing zeros
	s := fmt.Sprintf("%.15f", f)
	// Remove trailing zeros
	s = strings.TrimRight(s, "0")
	if strings.HasSuffix(s, ".") {
		s = s[:len(s)-1]
	}
	return NewFromString(s)
}

// New creates a new Decimal with specified value, scale, and precision
func New(value *big.Int, scale, precision int32) *Decimal {
	return &Decimal{
		value:     new(big.Int).Set(value),
		scale:     scale,
		precision: precision,
	}
}

// String returns the string representation of the decimal
func (d *Decimal) String() string {
	if d.value.Sign() == 0 {
		return "0"
	}

	// Convert to string
	s := d.value.String()

	// Handle negative numbers
	negative := false
	if strings.HasPrefix(s, "-") {
		negative = true
		s = s[1:]
	}

	// Add decimal point if needed
	if d.scale > 0 {
		// Pad with leading zeros if necessary
		for len(s) <= int(d.scale) {
			s = "0" + s
		}

		// Insert decimal point
		pointPos := len(s) - int(d.scale)
		s = s[:pointPos] + "." + s[pointPos:]
	}

	// Remove trailing zeros after decimal point
	if d.scale > 0 {
		s = strings.TrimRight(s, "0")
		if strings.HasSuffix(s, ".") {
			s = s[:len(s)-1]
		}
	}

	// Add negative sign if needed
	if negative {
		s = "-" + s
	}

	return s
}

// Add adds two decimals and returns the result
func (d *Decimal) Add(other *Decimal) *Decimal {
	// Align scales
	aligned1, aligned2 := d.alignScales(other)

	// Add values using pooled big.Int
	result := getBigInt()
	result.Add(aligned1.value, aligned2.value)

	// Use the larger scale
	maxScale := d.scale
	if other.scale > maxScale {
		maxScale = other.scale
	}

	// Calculate precision
	precision := max(d.precision, other.precision)

	return &Decimal{
		value:     result,
		scale:     maxScale,
		precision: precision,
	}
}

// Sub subtracts other from d and returns the result
func (d *Decimal) Sub(other *Decimal) *Decimal {
	// Align scales
	aligned1, aligned2 := d.alignScales(other)

	// Subtract values using pooled big.Int
	result := getBigInt()
	result.Sub(aligned1.value, aligned2.value)

	// Use the larger scale
	maxScale := d.scale
	if other.scale > maxScale {
		maxScale = other.scale
	}

	// Calculate precision
	precision := max(d.precision, other.precision)

	return &Decimal{
		value:     result,
		scale:     maxScale,
		precision: precision,
	}
}

// Mul multiplies two decimals and returns the result
func (d *Decimal) Mul(other *Decimal) *Decimal {
	// Multiply values using pooled big.Int
	result := getBigInt()
	result.Mul(d.value, other.value)

	// Add scales
	newScale := d.scale + other.scale

	// Calculate precision
	precision := d.precision + other.precision

	return &Decimal{
		value:     result,
		scale:     newScale,
		precision: precision,
	}
}

// Div divides d by other and returns the result
func (d *Decimal) Div(other *Decimal) (*Decimal, error) {
	if other.value.Sign() == 0 {
		return nil, fmt.Errorf("division by zero")
	}

	// For division, we need to scale up the dividend to get more precision
	// We'll use a scale that gives us reasonable precision (3 extra decimal places for rounding)
	extraPrecision := int32(3)
	targetScale := max(d.scale, other.scale) + extraPrecision

	// Scale up the dividend (numerator) to the target scale
	scaleDiff := targetScale - d.scale
	scaled1 := getBigInt()
	scaled1.Set(d.value)

	if scaleDiff > 0 {
		multiplier := getBigInt()
		multiplier.SetInt64(10)
		multiplier.Exp(multiplier, big.NewInt(int64(scaleDiff)), nil)
		scaled1.Mul(scaled1, multiplier)
		putBigInt(multiplier)
	}

	// Keep the divisor (denominator) at its original scale
	scaled2 := getBigInt()
	scaled2.Set(other.value)

	// Perform division using pooled big.Int
	result := getBigInt()
	result.Div(scaled1, scaled2)

	// The result scale is the target scale minus the divisor's scale
	resultScale := targetScale - other.scale

	// Calculate precision
	precision := max(d.precision, other.precision)

	// Return temporary big.Ints to pool
	putBigInt(scaled1)
	putBigInt(scaled2)

	return &Decimal{
		value:     result,
		scale:     resultScale,
		precision: precision,
	}, nil
}

// Cmp compares d with other and returns:
// -1 if d < other
//
//	0 if d == other
//	1 if d > other
func (d *Decimal) Cmp(other *Decimal) int {
	// Align scales
	aligned1, aligned2 := d.alignScales(other)

	return aligned1.value.Cmp(aligned2.value)
}

// Equals returns true if d equals other
func (d *Decimal) Equals(other *Decimal) bool {
	return d.Cmp(other) == 0
}

// IsZero returns true if d is zero
func (d *Decimal) IsZero() bool {
	return d.value.Sign() == 0
}

// IsPositive returns true if d is positive
func (d *Decimal) IsPositive() bool {
	return d.value.Sign() > 0
}

// IsNegative returns true if d is negative
func (d *Decimal) IsNegative() bool {
	return d.value.Sign() < 0
}

// Scale returns the scale of the decimal
func (d *Decimal) Scale() int32 {
	return d.scale
}

// Precision returns the precision of the decimal
func (d *Decimal) Precision() int32 {
	return d.precision
}

// Value returns the underlying big.Int value
func (d *Decimal) Value() *big.Int {
	return new(big.Int).Set(d.value)
}

// alignScales aligns the scales of two decimals for arithmetic operations
func (d *Decimal) alignScales(other *Decimal) (*Decimal, *Decimal) {
	if d.scale == other.scale {
		return d, other
	}

	// Find the maximum scale
	maxScale := d.scale
	if other.scale > maxScale {
		maxScale = other.scale
	}

	// Scale up the decimal with smaller scale
	scaleDiff1 := maxScale - d.scale
	scaleDiff2 := maxScale - other.scale

	aligned1 := &Decimal{
		value:     new(big.Int).Set(d.value),
		scale:     maxScale,
		precision: d.precision,
	}
	aligned2 := &Decimal{
		value:     new(big.Int).Set(other.value),
		scale:     maxScale,
		precision: other.precision,
	}

	// Scale up by multiplying by 10^scaleDiff
	if scaleDiff1 > 0 {
		multiplier := big.NewInt(10)
		multiplier.Exp(multiplier, big.NewInt(int64(scaleDiff1)), nil)
		aligned1.value.Mul(aligned1.value, multiplier)
	}

	if scaleDiff2 > 0 {
		multiplier := big.NewInt(10)
		multiplier.Exp(multiplier, big.NewInt(int64(scaleDiff2)), nil)
		aligned2.value.Mul(aligned2.value, multiplier)
	}

	return aligned1, aligned2
}

// isValidDecimalString checks if a string is a valid decimal representation
func isValidDecimalString(s string) bool {
	if s == "" {
		return false
	}

	// Check for valid characters
	for _, c := range s {
		if c != '-' && c != '.' && (c < '0' || c > '9') {
			return false
		}
	}

	// Check for multiple decimal points
	dotCount := strings.Count(s, ".")
	if dotCount > 1 {
		return false
	}

	// Check for multiple minus signs
	minusCount := strings.Count(s, "-")
	if minusCount > 1 {
		return false
	}

	// Check minus sign position
	if minusCount == 1 && !strings.HasPrefix(s, "-") {
		return false
	}

	return true
}

// countDigits counts the number of digits in an integer
func countDigits(n int64) int32 {
	if n == 0 {
		return 1
	}

	count := int32(0)
	if n < 0 {
		n = -n
	}

	for n > 0 {
		n /= 10
		count++
	}

	return count
}

// max returns the maximum of two int32 values
func max(a, b int32) int32 {
	if a > b {
		return a
	}
	return b
}

// Zero returns a decimal representing zero
func Zero() *Decimal {
	return &Decimal{
		value:     big.NewInt(0),
		scale:     0,
		precision: 1,
	}
}

// One returns a decimal representing one
func One() *Decimal {
	return &Decimal{
		value:     big.NewInt(1),
		scale:     0,
		precision: 1,
	}
}
