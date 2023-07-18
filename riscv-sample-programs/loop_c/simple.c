int func(int count ){
    int s = 0;
    for (int i = 1; i <= count; i++){
        s += i;
        s += i;
    }
    return s;
}

int main(int count){
    int r = func(count);
    return r;
}