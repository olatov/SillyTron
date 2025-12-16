int a;
int b;

int main() {
  // equal values
  a = 5; b = 5;
  printf("a=5 b=5");
  printf(a == b);
  printf(a != b);
  printf(a < b);
  printf(a <= b);
  printf(a > b);
  printf(a >= b);

  // a < b
  a = 5; b = 6;
  printf("a=5 b=6");
  printf(a == b);
  printf(a != b);
  printf(a < b);
  printf(a <= b);
  printf(a > b);
  printf(a >= b);

  // negative and zero
  a = -1; b = 0;
  printf("a=-1 b=0");
  printf(a == b);
  printf(a != b);
  printf(a < b);
  printf(a <= b);
  printf(a > b);
  printf(a >= b);

  // edge extremes (safe negative value)
  a = 32767; b = -32767;
  printf("a=32767 b=-32767");
  printf(a == b);
  printf(a != b);
  printf(a < b);
  printf(a <= b);
  printf(a > b);
  printf(a >= b);

  return 0;
}
