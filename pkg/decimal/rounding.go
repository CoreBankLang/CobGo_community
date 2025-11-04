package decimal

import (
	"github.com/shopspring/decimal"
)

// RoundingMode represents COBOL rounding modes
type RoundingMode int

const (
	// AWAY_FROM_ZERO - Round away from zero
	AWAY_FROM_ZERO RoundingMode = iota

	// NEAREST_AWAY_FROM_ZERO - Round to nearest, ties away from zero (COBOL DEFAULT)
	NEAREST_AWAY_FROM_ZERO

	// NEAREST_EVEN - Banker's rounding (ties to even)
	NEAREST_EVEN

	// NEAREST_TOWARD_ZERO - Round to nearest, ties toward zero
	NEAREST_TOWARD_ZERO

	// PROHIBITED - Error if rounding needed
	PROHIBITED

	// TOWARD_GREATER - Always round up (ceiling)
	TOWARD_GREATER

	// TOWARD_LESSER - Always round down (floor)
	TOWARD_LESSER

	// TRUNCATION - Truncate (no rounding)
	TRUNCATION

	// MODE_ROUND - Implementation-defined (use NEAREST_EVEN)
	MODE_ROUND

	// DEFAULT - Standard COBOL rounding (NEAREST_AWAY_FROM_ZERO)
	DEFAULT
)

// Round rounds a decimal according to COBOL rules
func Round(d decimal.Decimal, places int32, mode RoundingMode) (decimal.Decimal, error) {
	switch mode {
	case AWAY_FROM_ZERO:
		return roundAwayFromZero(d, places), nil

	case NEAREST_AWAY_FROM_ZERO, DEFAULT:
		return roundNearestAwayFromZero(d, places), nil

	case NEAREST_EVEN, MODE_ROUND:
		return d.Round(places), nil // shopspring uses banker's rounding

	case NEAREST_TOWARD_ZERO:
		return roundNearestTowardZero(d, places), nil

	case PROHIBITED:
		// Check if rounding would be needed
		rounded := d.Round(places)
		if !rounded.Equal(d) {
			return d, ErrRoundingProhibited
		}
		return d, nil

	case TOWARD_GREATER:
		return d.RoundCeil(places), nil

	case TOWARD_LESSER:
		return d.RoundFloor(places), nil

	case TRUNCATION:
		return d.Truncate(places), nil

	default:
		return d, ErrInvalidRoundingMode
	}
}

// roundAwayFromZero rounds away from zero
func roundAwayFromZero(d decimal.Decimal, places int32) decimal.Decimal {
	if d.IsNegative() {
		return d.RoundFloor(places)
	}
	return d.RoundCeil(places)
}

// roundNearestAwayFromZero rounds to nearest, ties away from zero (COBOL default)
func roundNearestAwayFromZero(d decimal.Decimal, places int32) decimal.Decimal {
	// Get the digit at the rounding position
	shift := decimal.NewFromInt(10).Pow(decimal.NewFromInt32(places + 1))
	shifted := d.Mul(shift)

	// Check if we're exactly at .5
	fractional := shifted.Sub(shifted.Truncate(0))
	half := decimal.NewFromFloat(0.5)

	if fractional.Abs().Equal(half) {
		// Exactly at tie - round away from zero
		return roundAwayFromZero(d, places)
	}

	// Not a tie - use standard rounding
	return d.Round(places)
}

// roundNearestTowardZero rounds to nearest, ties toward zero
func roundNearestTowardZero(d decimal.Decimal, places int32) decimal.Decimal {
	// Get the digit at the rounding position
	shift := decimal.NewFromInt(10).Pow(decimal.NewFromInt32(places + 1))
	shifted := d.Mul(shift)

	// Check if we're exactly at .5
	fractional := shifted.Sub(shifted.Truncate(0))
	half := decimal.NewFromFloat(0.5)

	if fractional.Abs().Equal(half) {
		// Exactly at tie - round toward zero (truncate)
		return d.Truncate(places)
	}

	// Not a tie - use standard rounding
	return d.Round(places)
}

// RoundingModeFromString converts string to RoundingMode
func RoundingModeFromString(s string) (RoundingMode, error) {
	switch s {
	case "AWAY-FROM-ZERO":
		return AWAY_FROM_ZERO, nil
	case "NEAREST-AWAY-FROM-ZERO", "DEFAULT":
		return NEAREST_AWAY_FROM_ZERO, nil
	case "NEAREST-EVEN":
		return NEAREST_EVEN, nil
	case "NEAREST-TOWARD-ZERO":
		return NEAREST_TOWARD_ZERO, nil
	case "PROHIBITED":
		return PROHIBITED, nil
	case "TOWARD-GREATER":
		return TOWARD_GREATER, nil
	case "TOWARD-LESSER":
		return TOWARD_LESSER, nil
	case "TRUNCATION":
		return TRUNCATION, nil
	case "MODE-ROUND":
		return MODE_ROUND, nil
	default:
		return DEFAULT, ErrInvalidRoundingMode
	}
}

// String returns the string representation of a rounding mode
func (r RoundingMode) String() string {
	switch r {
	case AWAY_FROM_ZERO:
		return "AWAY-FROM-ZERO"
	case NEAREST_AWAY_FROM_ZERO:
		return "NEAREST-AWAY-FROM-ZERO"
	case NEAREST_EVEN:
		return "NEAREST-EVEN"
	case NEAREST_TOWARD_ZERO:
		return "NEAREST-TOWARD-ZERO"
	case PROHIBITED:
		return "PROHIBITED"
	case TOWARD_GREATER:
		return "TOWARD-GREATER"
	case TOWARD_LESSER:
		return "TOWARD-LESSER"
	case TRUNCATION:
		return "TRUNCATION"
	case MODE_ROUND:
		return "MODE-ROUND"
	case DEFAULT:
		return "DEFAULT"
	default:
		return "UNKNOWN"
	}
}
