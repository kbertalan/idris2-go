package support

import (
	"fmt"
	"math"
	"math/big"
	"unicode/utf8"
)

type Value struct {
	Tag  int
	Args []any
}

func AsValue(v any) Value {
	return v.(Value)
}

func AsValuePtr(v any) *Value {
	if v == nil {
		return nil
	}
	return v.(*Value)
}

func Constructor(tag int, args ...any) Value {
	return Value{
		Tag:  tag,
		Args: args,
	}
}

func ConstructorPtr(tag int, args ...any) *Value {
	return &Value{
		Tag:  tag,
		Args: args,
	}
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

func Delay(fn func() any) any {
	var (
		result    any = nil
		completed     = false
	)

	return func() any {
		if completed {
			return result
		}
		result = fn()
		completed = true
		return result
	}
}

type IntegerType any

func IntegerLiteral(i string) IntegerType {
	z := &big.Int{}
	n, ok := z.SetString(i, 10)
	if !ok {
		panic(fmt.Sprintf("could not create big.Int from %s", i))
	}
	if n.IsInt64() {
		return n.Int64()
	}
	return n
}

func integerAdd(i, j int64) IntegerType {
	if i >= 0 && j >= 0 {
		if i <= math.MaxInt64-j {
			return i + j
		}
	}
	if i >= 0 && j < 0 || i < 0 && j >= 0 {
		return i + j
	}
	if i < 0 && j < 0 {
		if i >= math.MinInt64-j {
			return i + j
		}
	}

	z := big.Int{}
	return z.Add(big.NewInt(i), big.NewInt(j))
}

func IntegerAdd(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return integerAdd(i, j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := &big.Int{}
	r := z.Add(a, b)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func IntegerSub(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return integerAdd(i, -j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := &big.Int{}
	r := z.Sub(a, b)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func integerMul(i, j int64) IntegerType {
	if i == 0 || j == 0 {
		return int64(0)
	}
	if i > 0 && j > 0 {
		if i <= math.MaxInt64/j {
			return i * j
		}
	}
	if i > 0 && j < 0 {
		if i <= math.MinInt64/j {
			return i * j
		}
	}

	if i < 0 && j > 0 {
		if i >= math.MinInt64/j {
			return i * j
		}
	}

	if i < 0 && j < 0 {
		if i >= math.MaxInt64/j {
			return i * j
		}
	}

	z := &big.Int{}
	return z.Mul(big.NewInt(i), big.NewInt(j))
}

func IntegerMul(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return integerMul(i, j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := big.Int{}
	r := z.Mul(a, b)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func IntegerDiv(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return i / j
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := big.Int{}
	r := z.Div(a, b)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func IntegerMod(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return i % j
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := big.Int{}
	r := z.Mod(a, b)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func IntegerNeg(x any) IntegerType {
	var a *big.Int
	switch i := x.(type) {
	case int64:
		return -i
	case *big.Int:
		a = i
	}

	z := big.Int{}
	return z.Neg(a)
}

func integerShiftL(i int64, j uint) IntegerType {
	if j < 64 {
		hi := i >> (64 - j)
		lo := i << j
		if hi == 0 && lo >= 0 {
			return lo
		}
	}

	z := &big.Int{}
	r := z.Lsh(big.NewInt(i), j)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func IntegerShiftL(x, y any) IntegerType {
	var a *big.Int
	var b uint
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return integerShiftL(i, uint(j))
		case *big.Int:
			if !j.IsUint64() {
				panic("cannot shift left with big integer")
			}
			return integerShiftL(i, uint(j.Uint64()))
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = uint(j)
		case *big.Int:
			if !j.IsInt64() {
				panic("cannot shift left with big integer")
			}
			b = uint(j.Int64())
		}
	}

	z := big.Int{}
	r := z.Lsh(a, b)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func IntegerShiftR(x, y any) IntegerType {
	var a *big.Int
	var b uint
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return i >> uint(j)
		case *big.Int:
			if !j.IsUint64() {
				panic("cannot shift right with big integer")
			}
			return i >> uint(j.Uint64())
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = uint(j)
		case *big.Int:
			if !j.IsInt64() {
				panic("cannot shift right with big integer")
			}
			b = uint(j.Int64())
		}
	}

	z := big.Int{}
	r := z.Rsh(a, b)
	if r.IsInt64() {
		return r.Int64()
	}
	return r
}

func IntegerBAnd(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return i & j
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := big.Int{}
	return z.And(a, b)
}

func IntegerBOr(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return i | j
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := big.Int{}
	return z.Or(a, b)
}

func IntegerBXOr(x, y any) IntegerType {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return i ^ j
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	z := big.Int{}
	return z.Xor(a, b)
}

func BoolAsInt(b bool) uint8 {
	if b {
		return 1
	}
	return 0
}

func IntegerLT(x, y any) uint8 {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return BoolAsInt(i < j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	c := a.Cmp(b)
	return BoolAsInt(c < 0)
}

func IntegerLTE(x, y any) uint8 {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return BoolAsInt(i <= j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	c := a.Cmp(b)
	return BoolAsInt(c <= 0)
}

func IntegerEQ(x, y any) uint8 {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return BoolAsInt(i == j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	c := a.Cmp(b)
	return BoolAsInt(c == 0)
}

func IntegerGTE(x, y any) uint8 {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return BoolAsInt(i >= j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	c := a.Cmp(b)
	return BoolAsInt(c >= 0)
}

func IntegerGT(x, y any) uint8 {
	var a, b *big.Int
	switch i := x.(type) {
	case int64:
		switch j := y.(type) {
		case int64:
			return BoolAsInt(i > j)
		case *big.Int:
			a = big.NewInt(i)
			b = j
		}
	case *big.Int:
		a = i
		switch j := y.(type) {
		case int64:
			b = big.NewInt(j)
		case *big.Int:
			b = j
		}
	}

	c := a.Cmp(b)
	return BoolAsInt(c > 0)
}

func StrLength(v any) int {
	return utf8.RuneCountInString(v.(string))
}

func StrHead(v any) rune {
	r, _ := utf8.DecodeRuneInString(v.(string))
	return r
}

func StrTail(v any) string {
	str := []rune(v.(string))
	return string(str[1:])
}

func StrIndex(v, i any) rune {
	str := []rune(v.(string))
	idx := i.(int)
	return str[idx]
}

func StrCons(vch any, v any) string {
	ch := vch.(rune)
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

func StrSubstr(o, l, s any) string {
	offset := o.(int)
	length := l.(int)
	till := offset + length
	str := []rune(s.(string))
	strLen := len(str)
	if till > strLen {
		till = strLen
	}
	return string(str[offset:till])
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
