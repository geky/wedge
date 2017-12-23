

extern putchar = int -> int

type hello
    def a = int

def int_copy = int -> int
fun int_copy(a)
    return 0

def chars = void -> int, int, int
fun chars()
    return 72, 105, 33

export main
fun main()
    let a, b, c = chars()
    putchar(a)
    putchar(b)
    putchar(c)
    putchar(10)
    return 0

