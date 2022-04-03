void do_fma(int *a, int *b, int *c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    c[i] += a[i] * b[i];
}
