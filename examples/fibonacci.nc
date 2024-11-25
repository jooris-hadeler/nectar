fn fibonacci(n: u64) -> u64 {
    if n == 1 {
        return 1;
    } elif n == 2 {
        return 2;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}
