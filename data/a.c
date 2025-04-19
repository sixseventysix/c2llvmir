int add(int a, int b) {
    return -(a <= b);
}

int main() {
    int x = 2+(-5);
    x = 5;
    int y = add(x, 3);
    add(x,3);
    return y;
}