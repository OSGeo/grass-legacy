/* basic/counter.c --- macros (see basic.h) and routines for big counters */

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"

/*--------------------------------------------------------------------------*/

#define BUFFER_SIZE  30
#define DECI_LENGTH 25
#define BASE  1000000000  /* Assuming MAXINT == 2147483647 == 2^31 - 1 */
#define FORM  "%d%09d"

static char buffer[BUFFER_SIZE][DECI_LENGTH];
static int pointer = 0;

/*--------------------------------------------------------------------------*/

char *basic_counter (Basic_counter counter)
     /* Returns (pointer to) string with decimal representation of counter.
        This can be used in conjunction with print ("... %s ...", ...)
        statments; but not more than BUFFER_SIZE times per statement! */
{
  pointer ++;
  if (pointer == BUFFER_SIZE)
    pointer = 0;
  if (counter.b == 0)
    sprintf (buffer[pointer], "%d", counter.a);
  else 
    sprintf (buffer[pointer], FORM, counter.b, counter.a);
  return &(buffer[pointer][0]);
}

/*--------------------------------------------------------------------------*/

void basic_counter_reset (Basic_counter *counter)
{
  counter->a = counter->b = 0;
}

/*--------------------------------------------------------------------------*/

void basic_counter_plus_ (
     Basic_counter *counter,
     int increment)  /* increment >= 0 and "not too big" */
{
  int sum = counter->a + increment;
  if (sum > BASE)  /* this speeds up things; see below */ 
    {
      counter->a = sum mod BASE;
      counter->b += (sum / BASE);
    }
  else
    counter->a = sum;
}

/* BEGIN NOTE.

   This basic_counter_plus_() actually needs some time.
   Eg, delaunay() in detri.c...
   ___________________________________________________
   data___________int___unsigned_____w/o_if____with_if
   d/torus      15.46      14.18      21.15      16.43  CPU secs
   d/ra       4864.21    1764.64    2617.13    2013.23  CPU secs

   END NOTE. */
