#include "scanner.h"
#include "gis.h"
#include "cpoly.h"
#include "Vect.h"
#include "gbtree.h"
#include "parts.h"

/* write_lines.c */
int line_data_write(site_array *, line_array *, line_array *, ring_offsets *, d_type *,
		    d_type, int, int, int, char *, FILE *, char *);
int append_sring(line_set *, struct line_pnts *);
int mif_import_build_network(BTREE *, pntRepos *, cpolygon *);
int vertRegister(BTREE *, partDescript *, int);
int strip_duplicate_tracks(BTREE *);
int vbase_extract_lines(BTREE *, struct Map_info *);
int procSnapDistance(int, float *);
char *calcKeyValue( pntDescript *, double, int, double, double);
int btree_compare(char *, char *);
pntDescript *delete_track_in_circle(pntDescript *, int, double);
int add_link(pntDescript *, pntDescript *);
int delete_link(pntDescript *, pntDescript *);
double linear_track_length(pntDescript *, pntDescript *, int);
pntDescript *track_to_end(pntDescript *, int);
int delete_track(pntDescript *, pntDescript *, int);

/* parse.c */
int parse_all_fields(d_type *, field_data *, d_type, int, int *);

/* attval.c */
int get_att_field_val(const d_type *, const int, const int, FILE *);

/* fields.c */
void dump_field_info( void );


