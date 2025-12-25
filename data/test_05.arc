add :: (a: int, b: int) {
    c: int;
    c = a + b;
}

sum :: (a: int, b: int, c: int, d: int) int {
    result: int;
    result = a + b * c - d;
}

pointer_add :: (a: *int, offset: int) *int {	       
}

main :: ()  {
     x: int;
     x = sum(1, 2, 3, 4, 5);     
}