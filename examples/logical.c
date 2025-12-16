int a;
int b;

int main() {
  a = 1; b = 0;
  printf("a=1 b=0");
  printf(a && b);
  printf(a || b);
  printf(!a);
  printf(!b);

  a = 1; b = 1;
  printf("a=1 b=1");
  printf(a && b);
  printf(a || b);
  printf(!a);
  printf(!b);

  a = 0; b = 0;
  printf("a=0 b=0");
  printf(a && b);
  printf(a || b);
  printf(!a);
  printf(!b);

  a = 0; b = 1;
  printf("a=0 b=1");
  printf(a && b);
  printf(a || b);
  printf(!a);
  printf(!b);

  return 0;
}
