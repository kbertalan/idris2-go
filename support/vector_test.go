package support

import (
	"fmt"
	"testing"
)

func assertVector(t *testing.T, exp, got Vector) {
	t.Helper()
	expStr := fmt.Sprintf("%+v", exp)
	gotStr := fmt.Sprintf("%+v", got)
	if expStr != gotStr {
		t.Errorf("expected %s, but got %s", expStr, gotStr)
	}
}

func gen(from, to int) []any {
	res := make([]any, 0, to-from)
	for i := from; i <= to; i++ {
		res = append(res, i)
	}
	return res
}

func TestVector(t *testing.T) {
	t.Parallel()

	t.Run("append", func(t *testing.T) {
		t.Run("bits 1", func(t *testing.T) {
			t.Run("one item to empty vector", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 1,
					len:   1,
					data:  nil,
					tail:  []any{1},
				}
				v := NewVector(1)
				got := v.Append(1)

				assertVector(t, exp, got)
			})

			t.Run("one item to non-empty vector", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 1,
					len:   2,
					data:  nil,
					tail:  []any{1, 2},
				}
				v := NewVector(1).Append(1)
				got := v.Append(2)

				assertVector(t, exp, got)
			})

			t.Run("tail overflow", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 1,
					len:   3,
					data: []any{
						[]any{1, 2},
					},
					tail: []any{3},
				}
				v := NewVector(1).Append(1)
				got := v.Append(2, 3)

				assertVector(t, exp, got)
			})

			t.Run("double tail overflow", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 1,
					len:   6,
					data: []any{
						[]any{1, 2},
						[]any{3, 4},
					},
					tail: []any{5, 6},
				}
				v := NewVector(1).Append(1)
				got := v.Append(2, 3, 4, 5, 6)

				assertVector(t, exp, got)
			})

			t.Run("root overflow", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 2,
					len:   7,
					data: []any{
						[]any{
							[]any{1, 2},
							[]any{3, 4},
						},
						[]any{
							[]any{5, 6},
						},
					},
					tail: []any{7},
				}
				v := NewVector(1).Append(1, 2, 3, 4, 5, 6)
				got := v.Append(7)

				assertVector(t, exp, got)
			})

			t.Run("right tree", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 2,
					len:   9,
					data: []any{
						[]any{
							[]any{1, 2},
							[]any{3, 4},
						},
						[]any{
							[]any{5, 6},
							[]any{7, 8},
						},
					},
					tail: []any{9},
				}
				v := NewVector(1).Append(1, 2, 3, 4, 5, 6, 7, 8)
				got := v.Append(9)

				assertVector(t, exp, got)
			})

			t.Run("four levels", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 3,
					len:   11,
					data: []any{
						[]any{
							[]any{
								[]any{1, 2},
								[]any{3, 4},
							},
							[]any{
								[]any{5, 6},
								[]any{7, 8},
							},
						},
						[]any{
							[]any{
								[]any{9, 10},
							},
						},
					},
					tail: []any{11},
				}
				v := NewVector(1).Append(1, 2, 3, 4, 5, 6, 7, 8)
				got := v.Append(9, 10, 11)

				assertVector(t, exp, got)
			})

			t.Run("four levels - full", func(t *testing.T) {
				exp := vector{
					bits:  1,
					cover: 3,
					len:   18,
					data: []any{
						[]any{
							[]any{
								[]any{1, 2},
								[]any{3, 4},
							},
							[]any{
								[]any{5, 6},
								[]any{7, 8},
							},
						},
						[]any{
							[]any{
								[]any{9, 10},
								[]any{11, 12},
							},
							[]any{
								[]any{13, 14},
								[]any{15, 16},
							},
						},
					},
					tail: []any{17, 18},
				}
				v := NewVector(1).
					Append(1, 2, 3, 4, 5, 6, 7, 8).       // left
					Append(9, 10, 11, 12, 13, 14, 15, 16) // right
				got := v.Append(17, 18) // tail

				assertVector(t, exp, got)
			})
		})

		t.Run("bits 2", func(t *testing.T) {
			t.Run("two levels", func(t *testing.T) {
				exp := vector{
					bits:  2,
					cover: 2,
					len:   20,
					data: []any{
						[]any{1, 2, 3, 4},
						[]any{5, 6, 7, 8},
						[]any{9, 10, 11, 12},
						[]any{13, 14, 15, 16},
					},
					tail: []any{17, 18, 19, 20},
				}
				v := NewVector(2).
					Append(1, 2, 3, 4, 5, 6, 7, 8).
					Append(9, 10, 11, 12, 13, 14, 15, 16)
				got := v.Append(17, 18, 19, 20)

				assertVector(t, exp, got)
			})

			t.Run("three levels", func(t *testing.T) {
				exp := vector{
					bits:  2,
					cover: 4,
					len:   21,
					data: []any{
						[]any{
							[]any{1, 2, 3, 4},
							[]any{5, 6, 7, 8},
							[]any{9, 10, 11, 12},
							[]any{13, 14, 15, 16},
						},
						[]any{
							[]any{17, 18, 19, 20},
						},
					},
					tail: []any{21},
				}
				v := NewVector(2).
					Append(1, 2, 3, 4, 5, 6, 7, 8).
					Append(9, 10, 11, 12, 13, 14, 15, 16)
				got := v.Append(17, 18, 19, 20, 21)

				assertVector(t, exp, got)
			})
		})
	})

	t.Run("last", func(t *testing.T) {
		t.Run("empty", func(t *testing.T) {
			defer func() {
				if err := recover(); err == nil {
					t.Errorf("expected panic")
				}
			}()

			NewVector(5).Last()
		})

		t.Run("on tail", func(t *testing.T) {
			exp := 4
			v := NewVector(5).Append(gen(1, exp)...)
			got := v.Last()

			if exp != got {
				t.Errorf("expected %d, got %d in vector %+v", exp, got, v)
			}
		})

		t.Run("on tail, but has nodes", func(t *testing.T) {
			exp := 32 * 5
			v := NewVector(5).Append(gen(1, exp)...)
			got := v.Last()

			if exp != got {
				t.Errorf("expected %d, got %d in vector %+v", exp, got, v)
			}
		})
	})

	t.Run("get", func(t *testing.T) {
		t.Run("empty", func(t *testing.T) {
			defer func() {
				if err := recover(); err == nil {
					t.Fatalf("expected panic")
				}
			}()
			got := NewVector(5).Get(0)

			t.Errorf("expected panic, but got %+v", got)
		})

		t.Run("tail only", func(t *testing.T) {
			max := 32 - 1
			v := NewVector(5).Append(gen(0, max)...)
			for i := 0; i <= max; i++ {
				got := v.Get(i)
				if got != i {
					t.Errorf("expected %d, but got %d", i, got)
				}
			}
		})

		t.Run("non-empty in range", func(t *testing.T) {
			max := 32 * 100
			v := NewVector(5).Append(gen(0, max)...)
			for i := 0; i <= max; i++ {
				got := v.Get(i)
				if got != i {
					t.Errorf("expected %d, but got %d", i, got)
				}
			}
		})
	})

	t.Run("init", func(t *testing.T) {
		t.Run("empty", func(t *testing.T) {
			defer func() {
				if err := recover(); err != nil {
					// ignore
				}
			}()

			got := NewVector(5).Init()

			t.Errorf("expected panic, but got %+v", got)
		})

		t.Run("non-empty", func(t *testing.T) {
			v := NewVector(5).Append(1, 2, 3)
			exp := vectorView{v.(vector), 0, 2}
			got := v.Init()

			assertVector(t, exp, got)
		})
	})

	t.Run("dropFrom", func(t *testing.T) {

		t.Run("zero", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 1,
				len:   0,
				data:  nil,
				tail:  nil,
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(0)

			assertVector(t, exp, got)
		})

		t.Run("one", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 1,
				len:   1,
				data:  nil,
				tail:  []any{1},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(1)

			assertVector(t, exp, got)
		})

		t.Run("two", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 1,
				len:   2,
				data:  nil,
				tail:  []any{1, 2},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(2)

			assertVector(t, exp, got)
		})

		t.Run("three", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 1,
				len:   3,
				data: []any{
					[]any{1, 2},
				},
				tail: []any{3},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(3)

			assertVector(t, exp, got)
		})

		t.Run("four", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 1,
				len:   4,
				data: []any{
					[]any{1, 2},
				},
				tail: []any{3, 4},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(4)

			assertVector(t, exp, got)
		})

		t.Run("five", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 1,
				len:   5,
				data: []any{
					[]any{1, 2},
					[]any{3, 4},
				},
				tail: []any{5},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(5)

			assertVector(t, exp, got)
		})

		t.Run("six", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 1,
				len:   6,
				data: []any{
					[]any{1, 2},
					[]any{3, 4},
				},
				tail: []any{5, 6},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(6)

			assertVector(t, exp, got)
		})

		t.Run("seven", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 2,
				len:   7,
				data: []any{
					[]any{
						[]any{1, 2},
						[]any{3, 4},
					},
					[]any{
						[]any{5, 6},
					},
				},
				tail: []any{7},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(7)

			assertVector(t, exp, got)
		})

		t.Run("eight", func(t *testing.T) {
			exp := vector{
				bits:  1,
				cover: 2,
				len:   8,
				data: []any{
					[]any{
						[]any{1, 2},
						[]any{3, 4},
					},
					[]any{
						[]any{5, 6},
					},
				},
				tail: []any{7, 8},
			}
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(8)

			assertVector(t, exp, got)
		})

		t.Run("nine", func(t *testing.T) {
			v := NewVector(1).Append(gen(1, 8)...).(vector)
			got := v.dropFrom(9)

			assertVector(t, v, got)
		})

		t.Run("huge", func(t *testing.T) {
			max := 32 * 1000
			v := NewVector(5).Append(gen(0, max)...).(vector)
			for i := 1; i <= max; i++ {
				got := v.dropFrom(uint(i))

				expLast := i - 1
				last := got.Last()
				if last != expLast {
					t.Errorf("expected last %d, but got %d", expLast, last)
				}

				expLength := i
				length := got.Len()
				if length != expLength {
					t.Fatalf("expected length %d, but got %d", expLength, length)
				}
			}
		})
	})

	t.Run("appendAt", func(t *testing.T) {
		max := 32 * 1000
		items := gen(1, 10)
		v := NewVector(5).Append(gen(0, max)...).(vector)
		for i := 0; i <= max; i++ {
			got := v.appendAt(uint(i), items...)

			expLength := i + 10
			length := got.Len()
			if expLength != length {
				t.Fatalf("expected length %d, bug got %d", expLength, length)
			}
		}
	})

	t.Run("iter", func(t *testing.T) {
		iter := NewVector(5).Append(gen(0, 32000)...).Iter()
		for i := 0; iter.HasNext(); i++ {
			got := iter.Next()

			if i != got {
				t.Fatalf("expected %d, but got %d", i, got)
			}
		}
	})

	t.Run("reverseIter", func(t *testing.T) {
		max := 32
		iter := NewVector(5).Append(gen(0, max)...).ReverseIter()
		for i := max; iter.HasNext(); i-- {
			got := iter.Next()

			if i != got {
				t.Fatalf("expected %d, but got %d", i, got)
			}
		}
	})
}
