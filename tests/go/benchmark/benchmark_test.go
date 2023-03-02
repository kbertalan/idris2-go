package main

import "testing"

func BenchmarkFibonacci(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmark_fib(100)
	}
}

func BenchmarkBind(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmark_bind(100)
	}
}
