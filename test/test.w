import stdio

int test

def fix(void) -> void {
    test = 1+2 + 3+4
}

def +(int a, int b) -> int {
    return a
}

def !(int a) -> int {
    if (a) {
        return 0
    } else {
        return (1)
    }
}

def loop(void) -> void {
    while (!(test)) {}
}

def main(void) -> int {
    fix()
    printf("Hello World!\n\0")
    return 0
}

