
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

extern void initialize_scanner_string(const char *);
extern void initialize_scanner_stream(FILE *);

/* mapcalc.y */

extern expr_list *parse_string(const char *);
extern expr_list *parse_stream(FILE *);

/* expression.c */

extern int is_var(const char *);
extern void print_expression(FILE *, const expression *);

/* evaluate.c */

extern int execute(expr_list *);

/****************************************************************************/

#endif /* _MAPCALC_H_ */

