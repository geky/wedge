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
    def f = i(a), a -> int

type t(b, c)
    impl i(c)
    def x = b
    def y = c

def f(d, j, k) = t(d, j), k -> int
fun f(x, y)
    return 0

//interface i(a)
//    // NOTE TO SELF
//    // replacing `i(a)` with `u(c, d)` should work
//    // but you need to also carry over the fact that u
//    // is parameterized. You'll need to make f
//    // parameterized over `c` and `d`, as well as
//    // replace all instances of `a` with the arg to the
//    // `impl i` statment (in this case `d`)
//    // NOTE TO SELF
//    // Don't feel bad, it is very confusing. Do you
//    // think all interfaces should just be
//    // parameterized?
//    def f = i(a), a -> int
//
//type t(a)
//    impl i(a) // TODO do we need param here?
//    // translates to needing
//    // def f(a) = t(a) -> int
//    def x = a
//
//type u(c, d)
//    impl i(d)
//    def x = c
//    def y = d
//
//def f(a) = t(a), a -> int
//fun f(x, y)
//    return 0
//
////def f(a, b) = u(a, b), b -> int
////fun f(x, y)
////    return 12
////
//def g = i(int) -> int
//fun g(x)
//    return f(x, 0)

extern putchar = int -> int
export main
def main = void -> int
fun main()
    putchar(72)
    putchar(73)
    putchar(33)
    putchar(10)
    //g(t(int){0})
    //g(u(int, int){0, 0})
    return 0
