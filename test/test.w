import stdio

int test

def fix(void) -> void {
    test = 1
}

def not(int a) -> int {
    if (a) {
        return 0
    } else {
        return 1
    }
}

def loop(void) -> void {
    while (not(test)) {}
}

def main(void) -> int {
    fix()
    printf("Hello World!\n\0")
    return 0
}

