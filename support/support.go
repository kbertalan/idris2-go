package support

import (
	"math"
	"math/big"
)

type Value struct {
	Tag  int
	Args []any
}

func AsValue(v any) Value {
	return v.(Value)
}

func Constructor(tag int, args ...any) Value {
	return Value{
		Tag:  tag,
		Args: args,
	}
}

type IntegerType *big.Int

func IntegerLiteral(i int64) IntegerType {
	return big.NewInt(i)
}

type TypeValue int

const (
	InvalidTypeValue TypeValue = iota
	IntTypeValue
	Int8TypeValue
	Int16TypeValue
	Int32TypeValue
	Int64TypeValue
	IntegerTypeValue
	Bits8TypeValue
	Bits16TypeValue
	Bits32TypeValue
	Bits64TypeValue
	StringTypeValue
	CharTypeValue
	DoubleTypeValue
	WorldTypeValue
)

func IntegerAdd(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.Add(a, b)
}

func IntegerSub(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.Sub(a, b)
}

func IntegerMul(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.Mul(a, b)
}

func IntegerDiv(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.Div(a, b)
}

func IntegerMod(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.Mod(a, b)
}

func IntegerNeg(x any) IntegerType {
	a := x.(IntegerType)
	z := big.Int{}
	return z.Neg(a)
}

func IntegerShiftL(x, y any) IntegerType {
	a := x.(IntegerType)
	b := (*big.Int)(y.(IntegerType)).Uint64()
	z := big.Int{}
	return z.Lsh(a, uint(b))
}

func IntegerShiftR(x, y any) IntegerType {
	a := x.(IntegerType)
	b := (*big.Int)(y.(IntegerType)).Uint64()
	z := big.Int{}
	return z.Rsh(a, uint(b))
}

func IntegerBAnd(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.And(a, b)
}

func IntegerBOr(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.Or(a, b)
}

func IntegerBXOr(x, y any) IntegerType {
	a := x.(IntegerType)
	b := y.(IntegerType)
	z := big.Int{}
	return z.Xor(a, b)
}

func BoolAsInt(b bool) int {
	if b {
		return 1
	}
	return 0
}

func IntegerLT(x, y any) int {
	a := (*big.Int)(x.(IntegerType))
	b := y.(IntegerType)
	c := a.Cmp(b)
	return BoolAsInt(c < 0)
}

func IntegerLTE(x, y any) int {
	a := (*big.Int)(x.(IntegerType))
	b := y.(IntegerType)
	c := a.Cmp(b)
	return BoolAsInt(c <= 0)
}

func IntegerEQ(x, y any) int {
	a := (*big.Int)(x.(IntegerType))
	b := y.(IntegerType)
	c := a.Cmp(b)
	return BoolAsInt(c == 0)
}

func IntegerGTE(x, y any) int {
	a := (*big.Int)(x.(IntegerType))
	b := y.(IntegerType)
	c := a.Cmp(b)
	return BoolAsInt(c >= 0)
}

func IntegerGT(x, y any) int {
	a := (*big.Int)(x.(IntegerType))
	b := y.(IntegerType)
	c := a.Cmp(b)
	return BoolAsInt(c > 0)
}

func StrCons(vch any, v any) string {
	ch := rune(vch.(uint8))
	str := v.(string)
	return string([]rune{ch}) + str
}

func StrReverse(v any) string {
	str := v.(string)
	runes := make([]rune, 0, len(str))
	for _, r := range str {
		runes = append(runes, r)
	}

	l := len(runes)

	for i := 0; i < l/2; i++ {
		o := l - i - 1
		runes[i], runes[o] = runes[o], runes[i]
	}

	return string(runes)
}

func DoubleExp(x any) float64 {
	return math.Exp(x.(float64))
}

func DoubleLog(x any) float64 {
	return math.Log(x.(float64))
}

func DoublePow(x, y any) float64 {
	return math.Pow(x.(float64), y.(float64))
}

func DoubleSin(x any) float64 {
	return math.Sin(x.(float64))
}

func DoubleCos(x any) float64 {
	return math.Cos(x.(float64))
}

func DoubleTan(x any) float64 {
	return math.Tan(x.(float64))
}

func DoubleASin(x any) float64 {
	return math.Asin(x.(float64))
}

func DoubleACos(x any) float64 {
	return math.Acos(x.(float64))
}

func DoubleATan(x any) float64 {
	return math.Atan(x.(float64))
}

func DoubleSqrt(x any) float64 {
	return math.Sqrt(x.(float64))
}

func DoubleFloor(x any) float64 {
	return math.Floor(x.(float64))
}

func DoubleCeiling(x any) float64 {
	return math.Ceil(x.(float64))
}
