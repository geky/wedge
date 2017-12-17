

extern putchar = int -> int

def hi1 = int -> int, int, int
fun hi1(i)
    return 72, 105, 33

def hi = int -> int, int, int
fun hi(i)
    return hi1(i)

export main
def main = void -> int
fun main()
    let a, b, c = hi(0)
    putchar(a)
    putchar(b)
    putchar(c)
    putchar(10)
    return 0

