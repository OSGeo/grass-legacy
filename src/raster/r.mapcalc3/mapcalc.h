
#ifndef _MAPCALC_H_
#define _MAPCALC_H_

/****************************************************************************/

#include <stdio.h>

#include "config.h"
#include "gis.h"

#include "globals.h"
#include "expression.h"

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

extern void execute(expr_list *);

/* map.c */

extern int map_type(const char *name, int mod);
extern int open_map(const char *name, int mod, int row, int col);
extern void setup_maps(void);
extern void close_maps(void);
extern void get_map_row(int idx, int mod, int row, int col, void *buf, int res_type);

extern int open_output_map(const char *name, int res_type);
extern void put_map_row(int fd, void *buf, int res_type);
extern void close_output_map(int fd);

/****************************************************************************/

#endif /* _MAPCALC_H_ */

