#include "scanner.h"

/* write_lines.c */
int line_data_write(site_array *, line_array *, d_type *, d_type, int, int, char *, FILE *, char *);

/* parse.c */
int parse_all_fields(d_type *, field_data *, d_type, int, int *);

/* attval.c */
int get_att_field_val(const d_type *, const int, const int, FILE *);

/* fields.c */
void dump_field_info( void );


