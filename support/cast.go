package support

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"
)

// Int

type intType interface {
	int | int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64
}

func CastNumberToInt[T intType](x T) int {
	return int(x)
}

func CastNumberToInt8[T intType](x T) int8 {
	return int8(x)
}

func CastNumberToInt16[T intType](x T) int16 {
	return int16(x)
}

func CastNumberToInt32[T intType](x T) int32 {
	return int32(x)
}

func CastNumberToInt64[T intType](x T) int64 {
	return int64(x)
}

func CastNumberToUInt8[T intType](x T) uint8 {
	return uint8(x)
}

func CastNumberToUInt16[T intType](x T) uint16 {
	return uint16(x)
}

func CastNumberToUInt32[T intType](x T) uint32 {
	return uint32(x)
}

func CastNumberToUInt64[T intType](x T) uint64 {
	return uint64(x)
}

func CastNumberToInteger[T intType](x T) IntegerType {
	return big.NewInt(int64(x))
}

func CastNumberToString[T intType](x T) string {
	return fmt.Sprintf("%d", x)
}

func CastNumberToChar[T intType](x T) byte {
	return byte(x)
}

func CastNumberToFloat64[T intType](x T) float64 {
	return float64(x)
}

// Double

type doubleType interface {
	float32 | float64
}

func CastDoubleToInt[T doubleType](x T) int {
	return int(x)
}

func CastDoubleToInt8[T doubleType](x T) int8 {
	return int8(x)
}

func CastDoubleToInt16[T doubleType](x T) int16 {
	return int16(x)
}

func CastDoubleToInt32[T doubleType](x T) int32 {
	return int32(x)
}

func CastDoubleToInt64[T doubleType](x T) int64 {
	return int64(x)
}

func CastDoubleToUInt8[T doubleType](x T) uint8 {
	return uint8(x)
}

func CastDoubleToUInt16[T doubleType](x T) uint16 {
	return uint16(x)
}

func CastDoubleToUInt32[T doubleType](x T) uint32 {
	return uint32(x)
}

func CastDoubleToUInt64[T doubleType](x T) uint64 {
	return uint64(x)
}

func CastDoubleToInteger[T doubleType](x T) IntegerType {
	return big.NewInt(int64(x))
}

func CastDoubleToString[T doubleType](x T) string {
	str := strconv.FormatFloat(float64(x), 'f', -1, 64)
	if strings.IndexRune(str, '.') < 0 {
		return str + ".0"
	}
	return str
}

func CastDoubleToChar[T doubleType](x T) byte {
	return byte(x)
}

func CastDoubleToFloat64[T doubleType](x T) float64 {
	return float64(x)
}

// Char

func CastCharToInt(x byte) int {
	return int(x)
}

func CastCharToInt8(x byte) int8 {
	return int8(x)
}

func CastCharToInt16(x byte) int16 {
	return int16(x)
}

func CastCharToInt32(x byte) int32 {
	return int32(x)
}

func CastCharToInt64(x byte) int64 {
	return int64(x)
}

func CastCharToUInt8(x byte) uint8 {
	return uint8(x)
}

func CastCharToUInt16(x byte) uint16 {
	return uint16(x)
}

func CastCharToUInt32(x byte) uint32 {
	return uint32(x)
}

func CastCharToUInt64(x byte) uint64 {
	return uint64(x)
}

func CastCharToInteger(x byte) IntegerType {
	return big.NewInt(int64(x))
}

func CastCharToString(x byte) string {
	return string([]byte{x})
}

func CastCharToChar(x byte) byte {
	return byte(x)
}

func CastCharToFloat64(x byte) float64 {
	return float64(x)
}

// Integer

func asInt64(v any) int64 {
	switch a := v.(type) {
	case int64:
		return a
	case *big.Int:
		return a.Int64()
	}
	panic(fmt.Sprintf("unknown integer implementation: %T", v))
}

func asUint64(v any) uint64 {
	switch a := v.(type) {
	case int64:
		return uint64(a)
	case *big.Int:
		return a.Uint64()
	}
	panic(fmt.Sprintf("unknown integer implementation: %T", v))
}

func CastIntegerToInt(x any) int {
	return int(asInt64(x))
}

func CastIntegerToInt8(x any) int8 {
	return int8(asInt64(x))
}

func CastIntegerToInt16(x any) int16 {
	return int16(asInt64(x))
}

func CastIntegerToInt32(x any) int32 {
	return int32(asInt64(x))
}

func CastIntegerToInt64(x any) int64 {
	return asInt64(x)
}

func CastIntegerToUInt8(x any) uint8 {
	return uint8(asUint64(x))
}

func CastIntegerToUInt16(x any) uint16 {
	return uint16(asUint64(x))
}

func CastIntegerToUInt32(x any) uint32 {
	return uint32(asUint64(x))
}

func CastIntegerToUInt64(x any) uint64 {
	return uint64(asUint64(x))
}

func CastIntegerToInteger(x any) IntegerType {
	switch a := x.(type) {
	case int64:
		return a
	case *big.Int:
		return a
	}
	panic(fmt.Sprintf("unknown integer implementation: %T", x))
}

func CastIntegerToString(x any) string {
	switch a := x.(type) {
	case int64:
		return CastNumberToString(a)
	case *big.Int:
		return a.String()
	}
	panic(fmt.Sprintf("unknown integer implementation: %T", x))
}

func CastIntegerToChar(x any) byte {
	return byte(asInt64(x))
}

func CastIntegerToFloat64(x any) float64 {
	return float64(asInt64(x))
}

// String

func CastStringToInt(x any) int {
	return int(must(strconv.ParseInt(x.(string), 10, 0)))
}

func CastStringToInt8(x any) int8 {
	return int8(must(strconv.ParseInt(x.(string), 10, 8)))
}

func CastStringToInt16(x any) int16 {
	return int16(must(strconv.ParseInt(x.(string), 10, 16)))
}

func CastStringToInt32(x any) int32 {
	return int32(must(strconv.ParseInt(x.(string), 10, 32)))
}

func CastStringToInt64(x any) int64 {
	return must(strconv.ParseInt(x.(string), 10, 64))
}

func CastStringToUInt8(x any) uint8 {
	return uint8(must(strconv.ParseUint(x.(string), 10, 8)))
}

func CastStringToUInt16(x any) uint16 {
	return uint16(must(strconv.ParseUint(x.(string), 10, 16)))
}

func CastStringToUInt32(x any) uint32 {
	return uint32(must(strconv.ParseUint(x.(string), 10, 32)))
}

func CastStringToUInt64(x any) uint64 {
	return uint64(must(strconv.ParseUint(x.(string), 10, 64)))
}

func CastStringToInteger(x any) IntegerType {
	return IntegerLiteral(x.(string))
}

func CastStringToString(x any) string {
	return x.(string)
}

func CastStringToFloat64(x any) float64 {
	return must(strconv.ParseFloat(x.(string), 64))
}

// utility

func must[T any](v T, err error) T {
	if err != nil {
		panic(err)
	}
	return v
}
