#ifndef SCANNER_INCLUDES_
#define SCANNER_INCLUDES_

#include "local_structs.h"

void parse_pen(char *);
int proc_pen_info(int, int *, int *, long *);
void parse_brush(char *);
int proc_brush_info(int, int *, long *, long *);
int alloc_site_space ( site_array *, int );
int alloc_glines ( line_array *, int );
int alloc_offsets ( ring_offsets *, int );
int append_simple_line(line_array *, const double, const double, const double, const double, const int);
type_array *type_array_init(void);
ring_offsets *offsets_init(void);
int append_type_spec(type_array *, const char, const int);
int append_offset(ring_offsets *, int, int);
int yywrap ();


#endif /* SCANNER_INCLUDES_ */
