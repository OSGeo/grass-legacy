/***********
* gsget.h
***********/

#include "gsurf.h"

/* good for types int, short, or char ONLY */
#define GET_MAPATT(buff, offset, att) att=(buff->ib? (float)buff->ib[offset]: \
				      buff->sb? (float)buff->sb[offset]: \
				      (float)buff->cb[offset])

#define SCALE_ATT(att, val, low, high)   ((val) <= att->max_nz && \
				(val) >= att->min_nz && att->range_nz? \
				(((val) - att->min_nz)/att->range_nz) * \
				((high) - (low)) + (low): 0)

/* cast to float, otherwise doesn't seem to handle neg. values */


#define XYMAXPOS 0x3ff    /* 1023 */
#define ZMAXPOS 0x3ff    /* 1023 */

#define NXMASK 0xffe00000/* top 11 bits */
#define NYMASK 0x1ffc00  /* middle 11 bits of packed int */
#define NZMASK 0x3ff     /* lowest 10 bits */

#define NZUP 0x000003ff

/* Fetch Normal vector from packed int */
#define FNORM(i,nv)  \
  nv[X] = ((int)(((i) & NXMASK) >> 21) - XYMAXPOS)/(float)XYMAXPOS; \
  nv[Y] = ((int)(((i) & NYMASK) >> 10) - XYMAXPOS)/(float)XYMAXPOS; \
  nv[Z] = (int)((i) & NZMASK)/(float)ZMAXPOS

/* Pack Normal vector into int */
#define PNORM(i,nv)  \
  i = ((unsigned int)((nv[X]*XYMAXPOS)+XYMAXPOS) << 21) |           \
  ((unsigned int)((nv[Y]*XYMAXPOS)+XYMAXPOS) << 10) |               \
  (unsigned int)(nv[Z]*ZMAXPOS)



