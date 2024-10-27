func fibo(n int) int {
	if n < 1 {
		return 0
	}
	if n < 3 {
		return 1
	}
	return fibo(n-1) + fibo(n-2)
}

print fibo(9)
