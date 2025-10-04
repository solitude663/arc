add :: (a: int, b: int) {
    c: int;
    c = a + b;
}

sum :: (a: int, b: int, c: int, d: int) {
    result: int;
    result = a + b * c - d;
}

main :: ()  {
     sum();     
}