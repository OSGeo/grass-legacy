/* lia/auxiliary.c  ---  Auxiliary routines for Lia. */

/*--------------------------------------------------------------------------*/

#include "base.h"

/*--------------------------------------------------------------------------*/

#define FPPLOAD_MAXLEN 20

Lia_info lia_common; /* NOTE: This is global because it's used internally
                        within the Lia Library; however, it isn't supposed
                        to be used from outside. (So, don't tell anybody. :) */

static int initialized = FALSE;

/*--------------------------------------------------------------------------*/

void lia_maximum (length)
     int length;
     /* Library initialization.
        Sets ABSOLUTE maximum number of Lia digits for lia_*() results.
        Can be called ONLY ONCE per execution!
        NOTE: A Lia object is Lia x[length] = x[0], x[1], ..., x[length-1]. */
{
  if (initialized)
    basic_error ("lia_maximum: called twice");
  lia_common.max = length;
  lia_common.length = length;
  lia_common.last = length - 1;
  basic_counter_reset (&lia_common.mul_calls);
  basic_counter_reset (&lia_common.mul_elops);
  basic_counter_reset (&lia_common.padd_calls);
  basic_counter_reset (&lia_common.padd_elops);
  basic_counter_reset (&lia_common.psub_calls);
  basic_counter_reset (&lia_common.psub_elops);
  lia_common.maximum_digit = 0;
  initialized = TRUE;
}

/*--------------------------------------------------------------------------*/

void lia_length (length)
     int length;
     /* Change the maximum length, and if want: library initialization.
        Can be called more then once, but cannot increase the ABSOLUTE maximum,
        either set by lia_maximum() or by FIRST call of lia_length(). */
{
  if (not initialized)
    { /* (better call lia_maximum() first :) */
      lia_maximum (length);
    }
  lia_common.length = length;
  lia_common.last = length - 1;
  if (lia_common.last >= lia_common.max)
    basic_error ("lia_length: length (%d) > maximum (%d)",
                length, lia_common.max);
}

/*--------------------------------------------------------------------------*/

int lia_get_maximum ()
     /* Returns the absolut maximum length; cf, lia_maximum(). */
{
  return (lia_common.max);
}

/*--------------------------------------------------------------------------*/

int lia_get_length ()
     /* Returns current maximum length; cf, lia_maximum() and lia_length(). */
{
  return (lia_common.length);
}

/*--------------------------------------------------------------------------*/

int lia_get_beta ()
     /* Returns the BETA value of the implementation.
        Notice, that BASE == 2^BETA and DBASE == BASE^2. */
{
  return (BETA);
}

/*--------------------------------------------------------------------------*/

Lia_info* lia_info ()
     /* Returns some Lia info. */
     /* NOTE: The returned address, which points to the info structure,
              is a constant.  DO NOT FREE() IT and consider the fields
              of the structure as read-only. */
{
  static Lia_info li;
  li = lia_common;
  return (&li);
}

/*--------------------------------------------------------------------------*/

void lia_fput (file, longi)
     FILE *file;
     Lia_obj longi;
     /* Prints internal representation of longi to file. */
{
  int i;
  fprint (file, "%c:", If ((lia_sign (longi) == -1), '-', '+'));
  downfor (i, longi[0] div_2, 1)
    fprint (file, "%lu:", longi[i]);
  fprint (file, "[%lu]", longi[0] div_2);
}

/*--------------------------------------------------------------------------*/

double lia_real (longi)
     Lia_obj longi;
     /* Converts longi to floating-point.
        NOTE: (1) Not too fast!
              (2) Is it accurate ???
                  Should we modify it using sprint() & sscan() ??? */
{
  int i;
  double b = 1.0, r = 0.0;
  upfor (i, 1, longi[0] div_2)
    {
      r += longi[i] * b;
      b *= (double) DBASE;
    }
  return (r * (double) lia_sign (longi));
}

/*--------------------------------------------------------------------------*/

int lia_high (longi)
     Lia_obj longi;
     /* Returns number of highest lia_digit used in longi:
        longi == [0... lia_high (long)]. */
{
  return (longi[0] div_2);
}

/*--------------------------------------------------------------------------*/

Lia lia_digit (longi, d)
     Lia_obj longi;
     int d;
     /* Returns longi[d] without any further checking. */
{
 return (longi[d]);
}

/*--------------------------------------------------------------------------*/

void lia_load (longi, shorti)
     Lia_obj longi;  /* output */
     int shorti;
     /* Initializes a long integer Lia object: longi := shorti.
        Assumes at least longi[0..2]. */
{
  Lia abs_value = Abs (shorti);
  if (lia_common.last < 2)
    basic_error ("lia_load: length < 3");
  longi[1] = abs_value mod_DBASE;
  longi[2] = abs_value div_DBASE;
  longi[0] = If ((longi[2] == 0), 1 times_2, 2 times_2);
  if (shorti < 0)
    longi[0] ++;
}

/*--------------------------------------------------------------------------*/

void lia_strload (longi, string, frmt)
     Lia_obj longi;  /* output */
     char string[];
     char frmt[];
     /* Converts a string, in given format frmt, to a Lia object;
        longi := "%d" or "%x" */
{
  Lia basis[3], ziffer[3];
  char digit[2];
  int d, i = 0;
  int negative_flag = FALSE;
  static Lia *aux = NULL;
  if (not aux)
    {
      aux = MALLOC (Lia, lia_common.max);
      MARK (aux, -LIA_MAGIC);
    }
  if ((frmt[0] != '%') or (frmt[2] != 0))
    basic_error ("lia_strload: totally wrong frmt: \"%s\"", frmt);
  switch (frmt[1])
    {
     case 'd':
      lia_load (basis, 10);
      break;
     case 'x':
      lia_load (basis, 16);
      break;
     default:
      basic_error ("lia_strload: wrong format: \"%s\"", frmt);
    }
  lia_load (longi, 0);
  digit[1] = 0;
  while (string[i] == ' ')
    i ++;
  if (string[i] == '-')
    {
      negative_flag = TRUE;  i ++;
    }
  else if (string[i] == '+')
    i ++;
  while (isdigit (digit[0] = string[i++]))
    {
      sscan (digit, frmt, &d);
      lia_load (ziffer, d);
      lia_mul (aux, longi, basis);
      lia_add (longi, aux, ziffer);   /* longi := basis * longi + ziffer */
    }
  if (negative_flag)
    lia_chs (longi);
}

/*--------------------------------------------------------------------------*/

void lia_ffpload (longi, w, a, value)
     Lia_obj longi;  /* output */
     int w, a;
     double value;
     /* Floating-point to Lia conversion: longi := int (value * 10^a).
        That is, longi is given value in "fix-point" format "%<w>.<a>f"
        ... MULTIPLIED by 10^a.
        Assuming: a < w < 16, since 64-bit fp has approx 16 decimal digits. */
{
  char buf[FPPLOAD_MAXLEN];
  sprint (buf, "%0.1f", value * basic_ipower (10, a));
  if ((strlen (buf) > w + 1 + 2) or (w + 1 + 2 + 1 > FPPLOAD_MAXLEN))
    basic_error ("lia_ffpload: overflow");
  lia_strload (longi, buf, "%d");
}

/*--------------------------------------------------------------------------*/

Lia_ptr lia_const (i)
     int i;
     /* Returns pointer to TEMPORARY Lia object representing int i.
        It's the users responsibility to save it via lia_copy() or
        lia_assign(), where needed.  The resulting Lia object will
        need at most 3 Lia digits!  Be careful: Don't ever use the
        resulting pointer other than read-only! */
{
  static Lia aux[3];  /* 3 digits are enough for an int */
  lia_load (aux, i);
  return (aux);
}

/*--------------------------------------------------------------------------*/

void lia_sdiv (result, remainder, longi, shorti)
     Lia_obj result;   /* output */
     int * remainder;  /* output */
     Lia_obj longi;
     int shorti;
     /* Does simple divison  longi / shorti  with  0 < shorti < BASE.
        NOTE: The result and longi arguments may denote the same Lia object! */
{
  Lia i, r, s, t, e, f, u, v;
  int zero_flag = TRUE;  /* for the time being */
  int negative_flag = is_negative (longi);
  s = DBASE / shorti;
  t = DBASE mod shorti;
  r = 0;
  downfor (i, longi[0] div_2, 1)
    {
      e = longi[i] / shorti;
      f = longi[i] mod shorti;
      u = r * s + e;
      v = r * t + f;   /* NOTE: overflow in v is possible when shorti > BASE */
      result[i] = u + v / shorti;
      r = v mod shorti;
    }
  *remainder = If (negative_flag, -r, r);
  downfor (i, longi[0] div_2, 1)
    if (result[i] > 0)
      {
        result[0] = i times_2;
        zero_flag = FALSE;
        break;
      }
  if (negative_flag)
    result[0] ++;
  if (zero_flag)
    result[0] = 2;
}
