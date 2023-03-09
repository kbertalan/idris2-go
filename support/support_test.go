package support

import (
	"fmt"
	"math"
	"math/big"
	"reflect"
	"testing"
)

func TestIntegerAdd(t *testing.T) {
	bigAdd := func(a, b *big.Int) *big.Int {
		z := big.Int{}
		return z.Add(a, b)
	}

	tcs := []struct {
		a, b IntegerType
		exp  IntegerType
	}{
		{int64(2), int64(math.MaxInt64), bigAdd(big.NewInt(2), big.NewInt(math.MaxInt64))},
		{int64(-2), int64(math.MaxInt64), int64(math.MaxInt64 - 2)},
		{int64(2), int64(math.MinInt64), int64(math.MinInt64 + 2)},
		{int64(-2), int64(math.MinInt64), bigAdd(big.NewInt(-2), big.NewInt(math.MinInt64))},
		{int64(3), int64(math.MaxInt64 - 3), int64(3) + (math.MaxInt64 - 3)},
		{int64(3), int64(math.MaxInt64 - 3 - 1), int64(3) + (math.MaxInt64 - 3 - 1)},
		{int64(3), int64(math.MaxInt64 - 3 + 1), bigAdd(big.NewInt(3), big.NewInt(math.MaxInt64-3+1))},
		{int64(-3), int64(math.MaxInt64 - 3), int64(-3) + (math.MaxInt64 - 3)},
		{int64(-3), int64(math.MaxInt64 - 3 - 1), int64(-3) + (math.MaxInt64 - 3 - 1)},
		{int64(-3), int64(math.MaxInt64 - 3 + 1), int64(-3) + (math.MaxInt64 - 3 + 1)},
		{int64(3), int64(math.MinInt64 + 3), int64(3) + (math.MinInt64 + 3)},
		{int64(3), int64(math.MinInt64 + 3 + 1), int64(3) + (math.MinInt64 + 3 + 1)},
		{int64(3), int64(math.MinInt64 + 3 - 1), int64(3) + (math.MinInt64 + 3 - 1)},
		{int64(-3), int64(math.MinInt64 + 3), int64(-3) + (math.MinInt64 + 3)},
		{int64(-3), int64(math.MinInt64 + 3 + 1), int64(-3) + (math.MinInt64 + 3 + 1)},
		{int64(-3), int64(math.MinInt64 + 3 - 1), bigAdd(big.NewInt(-3), big.NewInt(math.MinInt64+3-1))},
		{int64(2), big.NewInt(100), int64(102)},
	}

	for _, tc := range tcs {
		tc := tc
		t.Run(fmt.Sprintf("%v + %v", tc.a, tc.b), func(t *testing.T) {
			got := IntegerAdd(tc.a, tc.b)
			if !reflect.DeepEqual(tc.exp, got) {
				t.Errorf("got %v %T, expected %v %T", got, got, tc.exp, tc.exp)
			}
		})
	}
}

func TestIntegerMul(t *testing.T) {
	bigMul := func(a, b *big.Int) *big.Int {
		z := big.Int{}
		return z.Mul(a, b)
	}

	tcs := []struct {
		a, b IntegerType
		exp  IntegerType
	}{
		{int64(2), int64(math.MaxInt64), bigMul(big.NewInt(2), big.NewInt(math.MaxInt64))},
		{int64(-2), int64(math.MaxInt64), bigMul(big.NewInt(-2), big.NewInt(math.MaxInt64))},
		{int64(2), int64(math.MinInt64), bigMul(big.NewInt(2), big.NewInt(math.MinInt64))},
		{int64(-2), int64(math.MinInt64), bigMul(big.NewInt(-2), big.NewInt(math.MinInt64))},
		{int64(3), int64(math.MaxInt64 / 3), int64(3) * (math.MaxInt64 / 3)},
		{int64(3), int64(math.MaxInt64/3 - 1), int64(3) * (math.MaxInt64/3 - 1)},
		{int64(3), int64(math.MaxInt64/3 + 1), bigMul(big.NewInt(3), big.NewInt(math.MaxInt64/3+1))},
		{int64(-3), int64(math.MaxInt64 / 3), int64(-3) * (math.MaxInt64 / 3)},
		{int64(-3), int64(math.MaxInt64/3 - 1), int64(-3) * (math.MaxInt64/3 - 1)},
		{int64(-3), int64(math.MaxInt64/3 + 1), bigMul(big.NewInt(-3), big.NewInt(math.MaxInt64/3+1))},
		{int64(3), int64(math.MinInt64 / 3), int64(3) * (math.MinInt64 / 3)},
		{int64(3), int64(math.MinInt64/3 + 1), int64(3) * (math.MinInt64/3 + 1)},
		{int64(3), int64(math.MinInt64/3 - 1), bigMul(big.NewInt(3), big.NewInt(math.MinInt64/3-1))},
		{int64(-3), int64(math.MinInt64 / 3), int64(-3) * (math.MinInt64 / 3)},
		{int64(-3), int64(math.MinInt64/3 + 1), int64(-3) * (math.MinInt64/3 + 1)},
		{int64(-3), int64(math.MinInt64/3 - 1), bigMul(big.NewInt(-3), big.NewInt(math.MinInt64/3-1))},
		{int64(2), big.NewInt(100), int64(200)},
	}

	for _, tc := range tcs {
		tc := tc
		t.Run(fmt.Sprintf("%v * %v", tc.a, tc.b), func(t *testing.T) {
			got := IntegerMul(tc.a, tc.b)
			if !reflect.DeepEqual(tc.exp, got) {
				t.Errorf("got %v %T, expected %v %T", got, got, tc.exp, tc.exp)
			}
		})
	}
}

func TestIntegerShiftR(t *testing.T) {
	bigLsh := func(a *big.Int, b uint) *big.Int {
		z := big.Int{}
		return z.Lsh(a, b)
	}

	tcs := []struct {
		a, b IntegerType
		exp  IntegerType
	}{
		{bigLsh(big.NewInt(1), 63), int64(1), int64(1) << 62},
		{bigLsh(big.NewInt(1), 64), int64(1), bigLsh(big.NewInt(1), 63)},
		{bigLsh(big.NewInt(1), 65), int64(1), bigLsh(big.NewInt(1), 64)},
		{bigLsh(big.NewInt(1), 128), int64(70), int64(1) << 58},
		{int64(-8), int64(2), int64(-2)},
		{integerMul(int64(math.MaxInt64), int64(4)), int64(2), int64(math.MaxInt64)},
	}

	for _, tc := range tcs {
		tc := tc
		t.Run(fmt.Sprintf("%v >> %v", tc.a, tc.b), func(t *testing.T) {
			got := IntegerShiftR(tc.a, tc.b)
			if !reflect.DeepEqual(tc.exp, got) {
				t.Errorf("got %v %T, expected %v %T", got, got, tc.exp, tc.exp)
			}
		})
	}
}
