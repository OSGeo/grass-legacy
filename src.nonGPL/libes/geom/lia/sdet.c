#if 0

/* lia/sdet.c --- Lia sign computation for determinants. */

/*--------------------------------------------------------------------------*/

#include "base.h"

/*--------------------------------------------------------------------------*/

/* workspace */
static Lia *aux_buffer = NULL;
#define AUX  &(aux_buffer[0])

/*--------------------------------------------------------------------------*/

void lia_sdet ()
     /* Module initialization: allocate aux_buffer space. */
{
  aux_buffer = MALLOC (Lia, lia_get_maximum ());
  BZERO (aux_buffer,   Lia, lia_get_maximum ());
  MARK (aux_buffer, -LIA_MAGIC);
}

/*--------------------------------------------------------------------------*/

int lia_sdet0 ()
{
  return (1);
}

/*--------------------------------------------------------------------------*/

int lia_sdet1 (a)
     Lia_obj a;
{
   return (lia_sign (a));
}

/*--------------------------------------------------------------------------*/

int lia_sdet2 (a, b,
               e, f)
     Lia_obj a, b,  e, f;
{
  lia_det2 (a, b,
            e, f,  AUX);
  return (lia_sign (AUX));
}

/*--------------------------------------------------------------------------*/

int lia_sdet3 (a, b, c,
               e, f, g,
               o, p, q)
     Lia_obj a, b, c,   e, f, g,   o, p, q;
{
  lia_det3 (a, b, c,
            e, f, g,
            o, p, q, AUX);
  return (lia_sign (AUX));
}

/*--------------------------------------------------------------------------*/

int lia_sdet4 (a, b, c, d,
               e, f, g, h,
               o, p, q, r,
               s, t, u, v)
     Lia_obj a, b, c, d,  e, f, g, h,  o, p, q, r,  s, t, u, v;
{
  lia_det4 (a, b, c, d,
            e, f, g, h, 
            o, p, q, r,
            s, t, u, v,  AUX);
  return (lia_sign (AUX));
}

#endif
