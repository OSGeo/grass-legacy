/* sos/internal.h --- Headerfile for internal SoS routines. */

/* For internal use only! */

#ifndef __SOS_INTERNAL_H__ /* Include this file only once! */
#define __SOS_INTERNAL_H__

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/sos.h"

/*--------------------------------------------------------------------------*/

#define SOS_MAGIC  130862004

typedef struct sos_common_type
{
  int n, d, len, lenp;
  double scale;
} SoS_common;

/*--------------------------------------------------------------------------*/

/* sos.c */
Lia_ptr sos_lia_0();
Lia_ptr sos_lia_0_0();
void    sos_set_last_star();
extern  SoS_common sos_common;

/*--------------------------------------------------------------------------*/

/* primitive.c */
void    sos_new_depth_counters();
int     sos_epsilon_compare();

/*--------------------------------------------------------------------------*/

#endif  /* #ifndef __SOS_INTERNAL_H__ */
