fn fizz_buzz(n: i64) -> i64 {
    let rem_by_3 = mod(n, 3);
    let rem_by_5 = mod(n, 5);

    if rem_by_3 and rem_by_5 {
        return -3;
    } elif rem_by_5 {
        return -2;
    } elif rem_by_3 {
        return -1;
    } else {
        return n;
    }
}
