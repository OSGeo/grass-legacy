#include <stdio.h>
static int n_index (char *, int);

char *
dig_float_point (
		  char *buf,
		  int size,
		  double num)
{
  char tmp[100];
  int whole, frac;

  sprintf (tmp, "%f", num);
  whole = n_index (tmp, '.');
  frac = size - whole - 1;
  sprintf (buf, "%*.*f", whole, frac, num);
  return (buf);
}

static int 
n_index (char *str, int chr)
{
  register int cnt;

  for (cnt = 0; *str; str++, cnt++)
    if (*str == chr)
      return (cnt);
  return (-1);
}
