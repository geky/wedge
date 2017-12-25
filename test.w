

extern putchar = int -> int

type hello
    def a = int

def hm = void -> hello
fun hm()
    return hello(0)

//fun hey(a)
//    return hi(a)

def chars = void -> int, int, int
fun chars()
    hi(hm())
    hi(0)
    return 72, 105, 33

def hi = hello -> int
fun hi(a)
    return 72

def hi = int -> int
fun hi(a)
    return 66

export main
fun main()
    let a, b, c = chars()
    putchar(a)
    putchar(b)
    putchar(c)
    let a = 10
    putchar(a)
    return 0

