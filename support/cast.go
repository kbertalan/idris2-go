package support

import (
	"fmt"
	"math/big"
	"strconv"
)

// Int

type numericType interface {
	int | int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64 | float32 | float64
}

func CastNumberToInt[T numericType](x T) int {
	return int(x)
}

func CastNumberToInt8[T numericType](x T) int8 {
	return int8(x)
}

func CastNumberToInt16[T numericType](x T) int16 {
	return int16(x)
}

func CastNumberToInt32[T numericType](x T) int32 {
	return int32(x)
}

func CastNumberToInt64[T numericType](x T) int64 {
	return int64(x)
}

func CastNumberToUInt8[T numericType](x T) uint8 {
	return uint8(x)
}

func CastNumberToUInt16[T numericType](x T) uint16 {
	return uint16(x)
}

func CastNumberToUInt32[T numericType](x T) uint32 {
	return uint32(x)
}

func CastNumberToUInt64[T numericType](x T) uint64 {
	return uint64(x)
}

func CastNumberToInteger[T numericType](x T) IntegerType {
	return big.NewInt(int64(x))
}

func CastNumberToString[T numericType](x T) string {
	return fmt.Sprintf("%v", x)
}

func CastNumberToChar[T numericType](x T) rune {
	return rune(x)
}

func CastNumberToFloat64[T numericType](x T) float64 {
	return float64(x)
}

// Integer

func asBigInt(v any) *big.Int {
	return (*big.Int)(v.(IntegerType))
}

func CastIntegerToInt(x any) int {
	return int(asBigInt(x).Int64())
}

func CastIntegerToInt8(x any) int8 {
	return int8(asBigInt(x).Int64())
}

func CastIntegerToInt16(x any) int16 {
	return int16(asBigInt(x).Int64())
}

func CastIntegerToInt32(x any) int32 {
	return int32(asBigInt(x).Int64())
}

func CastIntegerToInt64(x any) int64 {
	return asBigInt(x).Int64()
}

func CastIntegerToUInt8(x any) uint8 {
	return uint8(asBigInt(x).Uint64())
}

func CastIntegerToUInt16(x any) uint16 {
	return uint16(asBigInt(x).Uint64())
}

func CastIntegerToUInt32(x any) uint32 {
	return uint32(asBigInt(x).Uint64())
}

func CastIntegerToUInt64(x any) uint64 {
	return uint64(asBigInt(x).Uint64())
}

func CastIntegerToInteger(x any) IntegerType {
	return big.NewInt(CastIntegerToInt64(x))
}

func CastIntegerToString(x any) string {
	return asBigInt(x).String()
}

func CastIntegerToChar(x any) rune {
	return rune(asBigInt(x).Int64())
}

func CastIntegerToFloat64(x any) float64 {
	return float64(CastIntegerToInt64(x))
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
	return big.NewInt(CastStringToInt64(x))
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
