func fact(n int) int {
	if n <= 1 {
		return 1
	}
	return n * fact(n-1)
}

print fact(6)
