/* basic test/demo for basic_cb_scanlinef() */

#include "basic.h"

main ()
{
  int io, i, j, k;
  double d;
  float f;
  char s1[1000], s2[1000], s3[1000];
  loop
    {
      print ("ENTER: %s\n",
             "(int) (string) (int) (string) (double) (string) (int) (float)");
      io = basic_cb_scanlinef (stdin, "%d %s %d %s %lf %s %d %f",
                               &i, s1, &j, s2, &d, s3, &k, &f);
      if (io == EOF) break;
      print ("[%d]\n", basic_cb_len ());
      print ("(int) %d\n", i);
      print ("(string) %s\n", s1);
      print ("(int) %d\n", j);
      print ("(string) %s\n", s2);
      print ("(double) %f\n", d);
      print ("(string) %s\n", s3);
      print ("(int) %d\n", k);
      print ("(float) %f\n", f);
      if (io != 8) print ("\nERROR: (io = %d)\n\n", io);
    }
  return (0);
}
