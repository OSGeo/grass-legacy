/* lia/mint.h  ---  Headerfile to lia/mint.c.  Needs cc ... -lmp */

#if ! (defined (sun) || defined (sparc))

/* No!    MP(3X) (-lmp) package only runs on Suns! */

#else

#ifndef __LIA_MP_H__  /* Include only once! */
#define __LIA_MP_H__  1

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/lia.h"
#include <mp.h>

/*--------------------------------------------------------------------------*/

void lia_div();
MINT * lia_mint();
MINT * lia_amint();

/*--------------------------------------------------------------------------*/

#endif  /* #ifndef __LIA_MP_H__ */

#endif  /* #if ... #else */
