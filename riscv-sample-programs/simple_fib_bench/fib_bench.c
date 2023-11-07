volatile long fibonacci(long n){
    volatile long a = 1;
    volatile long b = 1;
    for (long i = 0; i < n;i++){
        long c = a + b;
        a = b;
        b = c;
    }
    return a;
}

atomic_int count = 0;

long main(long loop_count){
  int a;

    return r;
}