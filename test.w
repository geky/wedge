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

extern putchar = int -> int

interface q
    def d = q -> int

interface u
    def v = u -> int

struct a
    impl q
    def x = int

struct b
    impl q
    def x = int

struct c(a, b)
    impl q
    def x = int
    def y = a
    def z = b

def d = int -> int
fun d(x)
    return 72

def d = a -> int
fun d(x)
    return 73

//def d = b -> int
fun d(x)
    return 33

//def d = c(int, int) -> int
//fun d(x)
///    return 10

fun e(x)
    return d(x)

fun f(x)
    return e(x)

export main
def main = void -> int
fun main()
    putchar(e(0))
    putchar(e(a{0}))
    putchar(e(b{0}))
    putchar(e(c(int, int){0,0}))
    return 0

////interface gahe
////    def pgahc = gahe -> int
//
//type gaha
//    def blah = int
//
//type gahb
//    def blah = int
//
//def pgahc = gaha -> int
//fun pgahc(a)
//    return 1
//
//def pgahc = gahb -> int
//fun pgahc(b)
//    return 2
//
//def pgahf = gaha -> int
//fun pgahf(a)
//    return 3
//
//def pgahf = gahb -> int
//fun pgahf(b)
//    return 4
//
////def pgahd = gahe -> int
//fun pgahd(e)
//    return pgahc(e)
//
//interface hey
//    def hi = hey -> int
//
//type hello
//    impl hey
//    def a = int
//
//type howdy
//    impl hey
//    def a = int
//
//def hm = void -> hello
//fun hm()
//    return hello(0)
//
//def what = hey -> int
//fun what(a)
//    return hi(a)
//
//def trywhat = int -> int
//fun trywhat(a)
//    return what(a)
//
//def doit = hey -> int
//fun doit(a)
//    return hi(a)
//
//def chars = void -> int, int, int
//fun chars()
//    hi(hm())
//    hi(0)
//    return 72, 105, 33
//
//def hi = hello -> int
//fun hi(a)
//    return 72
//
//def hi = howdy -> int
//fun hi(a)
//    return 90
//
//def hi = int -> int
//fun hi(a)
//    return 66
//
//export main
//fun main()
//    let a, b, c = chars()
//    putchar(a)
//    putchar(b)
//    putchar(c)
//    let a = 10
//    putchar(a)
//    return 0
//
