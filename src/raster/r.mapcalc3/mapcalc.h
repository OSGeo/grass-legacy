
#ifndef _MAPCALC_H_
#define _MAPCALC_H_

/****************************************************************************/

#include <stdio.h>

#include "config.h"
#include "gis.h"

#include "globals.h"
#include "expression.h"
#include "map.h"

/****************************************************************************/

/* mapcalc.l */

extern void initialize_scanner(const char *s);

/* mapcalc.y */

extern expression *parse(const char *s);

/* expression.c */

extern int is_var(const char *);
extern void print_expression(FILE *fp, const expression *e);

/* evaluate.c */

extern int execute(expression *e);

/****************************************************************************/

#endif /* _MAPCALC_H_ */

