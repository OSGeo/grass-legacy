#if 0

/* lia/sdet_one.c  --- Lia sign computation for determinants. */

/* The determinants here are assumed to have 1's in their last column,
   so there are only n(n-1) parameters per n-by-n determinant. */

/*--------------------------------------------------------------------------*/

#include "base.h"

/*--------------------------------------------------------------------------*/

/* workspace */
#define AUX_SIZE  17
#define AUX(IND)  &(aux_buffer [li * IND])
static Lia *aux_buffer = NULL;
static int li;

/*--------------------------------------------------------------------------*/

void lia_sdet_one ()
     /* Module initialization: allocate aux_buffer space. */
{
  li = lia_get_maximum ();
  aux_buffer = MALLOC (Lia, AUX_SIZE * li);
  BZERO (aux_buffer,   Lia, AUX_SIZE * li);
  MARK (aux_buffer, -LIA_MAGIC);
}

/*--------------------------------------------------------------------------*/

int lia_sdet1one ()
     /* returns always +1 */
{
  return (1);
}

/*--------------------------------------------------------------------------*/

int lia_sdet2one (a,
                  e)
     Lia_obj a,  e;
     /* returns sign of | a 1 |
                        | e 1 | */
{
  lia_sub (AUX (0), a, e);
  return (lia_sign (AUX (0)));
}

/*--------------------------------------------------------------------------*/

int lia_sdet3one (a, b,
                  e, f,
                  s, t)
     Lia_obj a, b,  e, f,  s, t;
     /* returns sign of | a b 1 |
                        | e f 1 |
                        | s t 1 | */
{
  lia_sub (AUX (0), e, a);
  lia_sub (AUX (1), f, b);
  lia_sub (AUX (2), s, a);
  lia_sub (AUX (3), t, b);
  lia_det2 (AUX (0), AUX (1),  AUX (2), AUX (3),  AUX (4));
  return (lia_sign (AUX (4)));
}

/*--------------------------------------------------------------------------*/

int lia_sdet4one (a, b, c,
                  e, f, g,
                  s, t, u,
                  o, p, q)
     Lia_obj a, b, c,  e, f, g,  s, t, u,  o, p, q;
     /* returns sign of | a b c 1 |
                        | e f g 1 |
                        | s t u 1 |
                        | o p q 1 | */
{
  lia_sub (AUX (0), e, a);   /* 2nd line - 1st line */
  lia_sub (AUX (1), f, b);
  lia_sub (AUX (2), g, c);
  lia_sub (AUX (3), s, a);   /* 3rd line - 1st line */
  lia_sub (AUX (4), t, b);
  lia_sub (AUX (5), u, c);
  lia_sub (AUX (6), o, a);   /* 4th line - 1st line */
  lia_sub (AUX (7), p, b);
  lia_sub (AUX (8), q, c);
  lia_det3 (AUX (0), AUX (1), AUX (2),
            AUX (3), AUX (4), AUX (5),
            AUX (6), AUX (7), AUX (8),  AUX (9));
  lia_chs (AUX (9));
  return (lia_sign (AUX (9)));
}

/*--------------------------------------------------------------------------*/

int lia_sdet5one (a, b, c, d,
                  e, f, g, h,
                  s, t, u, v,
                  o, p, q, r,
                  x, y, z, w)
     Lia_obj a, b, c, d,  e, f, g, h,  s, t, u, v,  o, p, q, r,  x, y, z, w;
     /* returns sign of | a b c d 1 |
                        | e f g h 1 |
                        | s t u v 1 |
                        | o p q r 1 |
                        | x y z w 1 | */
{
  lia_sub (AUX (0),  e, a);    /* 2nd line - 1st line */
  lia_sub (AUX (1),  f, b);
  lia_sub (AUX (2),  g, c);
  lia_sub (AUX (3),  h, d);
  lia_sub (AUX (4),  s, a);   /* 3rd line - 1st line */
  lia_sub (AUX (5),  t, b);
  lia_sub (AUX (6),  u, c);
  lia_sub (AUX (7),  v, d);
  lia_sub (AUX (8),  o, a);   /* 4th line - 1st line */
  lia_sub (AUX (9),  p, b);
  lia_sub (AUX (10), q, c);
  lia_sub (AUX (11), r, d);
  lia_sub (AUX (12), x, a);   /* 5th line - 1st line */
  lia_sub (AUX (13), y, b);
  lia_sub (AUX (14), z, c);
  lia_sub (AUX (15), w, d);
  lia_det4 (AUX (0),  AUX (1),  AUX (2),  AUX (3),
            AUX (4),  AUX (5),  AUX (6),  AUX (7),
            AUX (8),  AUX (9),  AUX (10), AUX (11),
            AUX (12), AUX (13), AUX (14), AUX (15), AUX (16));
  return (lia_sign (AUX (16)));
}

#endif
