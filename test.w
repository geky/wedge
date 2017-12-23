

extern putchar = int -> int

type hello
    def a = int

def int_copy = int -> int
fun int_copy(a)
    return 0

fun chars(a)
    def b = int
    let b = a
    return 72, 105, 33

export main
fun main()
    let a, b, c = chars(1)
    putchar(a)
    putchar(b)
    putchar(c)
    putchar(10)
    return 0

