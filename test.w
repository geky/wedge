// steps for interface glory
// 1. Allow passing type in interface into interface function
//
//    interface a
//        def b = a -> int
//    type c
//        impl a
//    fun main()
//        b(c())
//    // remember this comes second to def b = c -> int!
//
// 2. Infer interface if possible
//
//    interface a
//        def b = a -> int
//    def c = a -> int
//    type d
//        blah
//    def b = d -> int
//    fun main()
//        c(d())
//    // comes last
//
// 3. Infer implicit interfaces
//    type a
//        blah
//    type b
//        blah
//    def c = a -> int
//    def c = b -> int
//    fun d(x)
//        return c(x)

interface i(a)
    def f = i(a) -> int

type t(a)
    impl i(a) // TODO do we need param here?
    // translates to needing
    // def f(a) = t(a) -> int
    def x = a

type u(a)
    impl i(a)
    def x = a

def f(a) = t(a) -> int
fun f(x)
    return 0

def f(a) = u(a) -> int
fun f(x)
    return 1

def g = i(int) -> int
fun g(x)
    return f(x)

extern putchar = int -> int
export main
def main = void -> int
fun main()
    putchar(72)
    putchar(73)
    putchar(33)
    putchar(10)
    g(t(int){0})
    g(u(int){0})
    return 0
