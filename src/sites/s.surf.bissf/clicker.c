/*-
 * G_clicker()
 * 
 * Print a clock hand (one of '|', '/', '-', '\') to stderr.
 * Used in place of G_percent for unknown number of iterations
 * 
 */
#include <stdio.h>

static prev = 0;

G_clicker ()
{
  int x;
  static char clicks[]="|/-\\"; 

  if (prev == -1 || prev == 3)
    x = 0;

  else
    x = prev + 1;

  fprintf (stderr, "%1c\b", clicks[x]);
  fflush (stderr);
  prev = x;
}
