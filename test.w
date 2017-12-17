

extern putchar = int -> int

def hi = int -> int, int, int
fun hi(i)
    return 72, 105, 33

export main
def main = void -> int
fun main()
    let a, b, c = hi(0)
    putchar(a)
    putchar(b)
    putchar(c)
    putchar(10)
    return 0

