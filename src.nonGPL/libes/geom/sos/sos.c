/* sos/sos.c  ---  The SoS Library kernel. */

/*---------------------------------------------------------------------------*/

#ifndef lint
 static char version[] = "@(#) SoS Library 0.98";
 static char author[] = "Ernst Mucke";
#include "copyright.h"
#endif

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/sos.h"
#include "internal.h"
     
/*--------------------------------------------------------------------------*/

#ifdef __DEBUG__
 /* Global flags, but only if debugging! */
 int sos_test_flag = FALSE;
 int sos_proto_flag = FALSE;
 int sos_proto_e_flag = FALSE;
#endif

#ifdef __SOS_TRACE__
 /* Globally accessible trace file! */
 FILE *sos_trace_file = NULL;
#endif

/*--------------------------------------------------------------------------*/

static Lia * parameter;
static Lia * parameter_0_d_plus_1;
static int last_star;
static int last_loc;
static int memory;

SoS_common sos_common;  /* common block for internal SoS use */

/*--------------------------------------------------------------------------*/

#define location_of(I,J)  (sos_common.lenp * (sos_common.d * (I) + (J) - 1))
/* Macro to access position in Lia parameter matrix. */

/*--------------------------------------------------------------------------*/

void sos_init (n, d, scale, len, lenp)
     int n, d;
     double scale;
     int len, lenp;
     /* Initializes SoS package.
        - Allocates dynamic array parameter[...] for the global Lia parameter
          matrix Pi[i,j], ie: sos_lia (i,j), with 1 <= i <= n and 1 <= j <= d.
        - It also allocates space for the *-row (index 0; see minor.c and
          lambda3_star.c) which has d+1 (!!!) entries; the first d entries
          are stored in the unused 0-row of the parameter matrix, and the
          last entry is stored in parameter_0_d_plus_1.  The *-row is
          accessible via sos_lia_0 (j) with 1 <= j <= d+1.
        - The parameter scale sets sos_scale(). This can be used to camouflage
          the  fact that SoS works internally with int parameters.
          The user can either ignore this feature or use the following
          convention:
                       "real" FP parameter [i,j] == sos_scale "*" sos_lia (i,j)
        */
{
  int m = basic_malloc_info ()->total;
  lia_length (len);
  sos_common.d = d;
  sos_common.n = n;
  sos_common.len = len;
  sos_common.lenp = lenp;
  sos_common.scale = scale;
  last_loc = location_of (sos_common.n, sos_common.d);
  last_star = sos_common.d;
  if (parameter)
    basic_error ("sos_init: called twice");
  parameter = MALLOC (Lia, last_loc + lenp);
  BZERO (parameter,   Lia, last_loc + lenp);
  MARK  (parameter, -SOS_MAGIC);
  parameter_0_d_plus_1 = MALLOC (Lia, lenp);
  BZERO (parameter_0_d_plus_1,   Lia, lenp);
  MARK  (parameter_0_d_plus_1, -SOS_MAGIC);
  lia_det ();
/*>  lia_sdet (); */
/*>  lia_sdet_one (); */
  sos_minor ();
  memory = basic_malloc_info ()->total - m;
  print ("SoS: matrix[%d,%d] @ %d Lia digits; lia_length (%d); %.3f Mb.\n",
         n, d, lenp, len, basic_mbytes (memory));
#ifdef __DEBUG__
  if (sos_test_flag)
    print ("SoS: Library compiled with -DEBUG and sos_test flag set!\n");
#endif
}

/*--------------------------------------------------------------------------*/

int sos_bytes ()
     /* Returns space requirement for SoS in bytes. */
{
  return (memory);
}

/*--------------------------------------------------------------------------*/

Lia_ptr sos_lia (i, j)
     int i, j;
     /* Returns pointer to Lia representation of parameter Pi[i,j] */
{
  int loc = location_of (i, j);
#ifdef __DEBUG__
  if (sos_test_flag)
    if ((loc < 0) or (loc > last_loc)
        or (i < 1) or (i > sos_common.n)
        or (j < 1) or (j > sos_common.d))
    basic_error ("sos_lia(%d,%d) %s: n=%d d=%d lenp=%d len=%d loc=%d last=%d",
                i, j, "index error", sos_common.n, sos_common.d,
                sos_common.lenp, sos_common.len, loc, last_loc);
#endif
  return (& (parameter[loc]));
}

/*--------------------------------------------------------------------------*/

Lia_ptr sos_lia_0 (j)
     int j;
     /* Returns pointer to Lia entry of *-row. Internal use only! */
{
  if (j == sos_common.d + 1)
    return (parameter_0_d_plus_1);
  else
    {
#ifdef __DEBUG__
      if (sos_test_flag)
        if ((j < 1) or (j > sos_common.d))
          basic_error ("sos_lia_0: index error");
#endif
      return (& (parameter [location_of (0, j)]));
    }
}

/*--------------------------------------------------------------------------*/

Lia_ptr sos_lia_0_0 ()
     /* For notational convenience, the [0] entry of the *-row equals
        the [last_star] entry of the *-row (see minor.c/lambda3_star.c).
        Internal use only! */
{
#if __DEBUG__
  if (sos_test_flag)
    if ((last_star < 1) or (last_star > sos_common.d + 1))
      basic_error ("sos_lia_0_0: wrong last_star index");
#endif
  return (sos_lia_0 (last_star));
}

/*--------------------------------------------------------------------------*/

double sos_scale ()
     /* Returns the scale parameter of sos_init(). */
{
  return (sos_common.scale);
}

/*--------------------------------------------------------------------------*/

void sos_set_last_star (j)
     int j;
{
  last_star = j;
}

/*--------------------------------------------------------------------------*/

void sos_param (i, j, longi)
     int i, j;
     Lia longi[];
     /* Load Lia object longi into SoS parameter Pi[i,j] */
{
  if ((i == 0)
      or (i < 1) or (i > sos_common.n)
      or (j < 1) or (j > sos_common.d))
    basic_error ("sos_param: wrong indices");
  if (lia_high (longi) > sos_common.lenp - 1)
    basic_error ("sos_param: Lia parameter too long");
  lia_assign (sos_lia (i, j), longi);
}
