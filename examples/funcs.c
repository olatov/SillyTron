void hello() {
    printf("Hello, World!");
}

int answer() {
    int result = 42;
    return result;
}

int main() {
    hello();
    int ans = answer() + 5;
    printf("Answer:");
    printf(ans);
    return 0;
}
