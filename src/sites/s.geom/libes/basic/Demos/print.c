/* basic test/demo for basic_cb_printf() */

#include "basic.h"

main ()
{
  int r = 0, i = -7, j = 16, k = 9;
  double d = -1.5, e = 1234.56789e12;
  basic_cb_clear ();
  r += basic_cb_printf ("BE%cIN\n%5d %-7d \"%7s\" %lf\n", 'g', 14, i, "Hi", d);
  r += basic_cb_printf ("%16e abrakadabra 0x%x\n%d is roman %R\n", e, j, k, k);
  r += basic_cb_printf ("and %d is 0b%b in binary\nEND %s TEST\n", j, j, "of");
  print ("%s[printed: %d, size: %d]\n", basic_cb_str (), r, basic_cb_size ());
  return (0);
}
