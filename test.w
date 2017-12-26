

extern putchar = int -> int

interface hey
    def hi = hey -> int
    def werk = hey -> int, int

def werk = howdy -> int, int
fun werk(a)
    return 1,2

def werk = hello -> int, int
fun werk(a)
    return 1,3

type hello
    impl hey
    def a = int

type howdy
    impl hey
    def a = int

def hm = void -> hello
fun hm()
    return hello(0)

def what = hey -> int
fun what(a)
    return hi(a)

def doit = hey -> int
fun doit(a)
    return hi(a)

def chars = void -> int, int, int
fun chars()
    hi(hm())
    hi(0)
    return 72, 105, 33

def hi = hello -> int
fun hi(a)
    return 72

def hi = howdy -> int
fun hi(a)
    return 90

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

