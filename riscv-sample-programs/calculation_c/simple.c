int func(){
    volatile int a = 5;
    volatile int b = 3;
    int c = a+b;
    int d = a-b;
    int e = a & b;
    int f = a | b;
    return c + d + e + f;
}

int main(){
    int r = func();
    return r;
}