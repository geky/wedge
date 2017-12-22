

extern putchar = int -> int

type hello
    def a = int
    def b = int
    def c = int

def int_copy = int -> int
fun int_copy(i)
    return 0 // huh, this infinite loops...

def chars = void -> int, int, int
fun chars()
    return 72, 105, 33

export main
def main = void -> int
fun main()
    let a, b, c = chars()
    putchar(a)
    putchar(b)
    putchar(c)
    putchar(10)
    return a

