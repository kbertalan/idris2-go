package support

import (
	"math"
	"math/big"
)

type Value struct {
	Tag  int
	Args []any
}

func Constructor(tag int, args ...any) any {
	return Value{
		Tag:  tag,
		Args: args,
	}
}

func AsValue(v any) Value {
	return v.(Value)
}

func IntegerLiteral(i int64) *big.Int {
	return big.NewInt(i)
}

func AsInteger(v any) *big.Int {
	return v.(*big.Int)
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

type WorldType struct{}

var World WorldType

func IntegerAdd(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.Add(a, b)
}

func IntegerSub(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.Sub(a, b)
}

func IntegerMul(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.Mul(a, b)
}

func IntegerDiv(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.Div(a, b)
}

func IntegerMod(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.Mod(a, b)
}

func IntegerNeg(x any) *big.Int {
	a := x.(*big.Int)
	z := big.Int{}
	return z.Neg(a)
}

func IntegerShiftL(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int).Uint64()
	z := big.Int{}
	return z.Lsh(a, uint(b))
}

func IntegerShiftR(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int).Uint64()
	z := big.Int{}
	return z.Rsh(a, uint(b))
}

func IntegerBAnd(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.And(a, b)
}

func IntegerBOr(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.Or(a, b)
}

func IntegerBXOr(x, y any) *big.Int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	z := big.Int{}
	return z.Xor(a, b)
}

func boolAsInt(b bool) int {
	if b {
		return 1
	}
	return 0
}

func IntegerLT(x, y any) int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	c := a.Cmp(b)
	return boolAsInt(c < 0)
}

func IntegerLTE(x, y any) int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	c := a.Cmp(b)
	return boolAsInt(c <= 0)
}

func IntegerEQ(x, y any) int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	c := a.Cmp(b)
	return boolAsInt(c == 0)
}

func IntegerGTE(x, y any) int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	c := a.Cmp(b)
	return boolAsInt(c >= 0)
}

func IntegerGT(x, y any) int {
	a := x.(*big.Int)
	b := y.(*big.Int)
	c := a.Cmp(b)
	return boolAsInt(c > 0)
}

func StrCons(ch rune, str string) string {
	return string([]rune{ch}) + str
}

func StrReverse(str string) string {
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
