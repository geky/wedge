

extern putchar = int -> int

type hello
    def a = int
    def b = int
    def c = int

def hi = hello, hello -> int, int, int
fun hi(i, h)
    hello(0, 1, 2)
    return 72, 105, 33

export main
def main = void -> int
fun main()
    let h = hello(1,2,3)
    let a, b, c = hi(hello(0, 1, 2), h)
    putchar(a)
    putchar(b)
    putchar(c)
    putchar(10)
    return 0

