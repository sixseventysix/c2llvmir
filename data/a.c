int add(int a, int b) {
    return -(a <= b);
}

int main() {
    int x = 2+(-5);
    char c = 'a';
    int y = add(x, 3);
    return y;
}