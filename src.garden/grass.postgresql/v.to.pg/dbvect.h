#include "vect/digit.h"
#include <libpq-fe.h>

#define QRY_LENGTH 1024
#define MAXFLSIZE 16 /* e.g., 630000.123456*/
#define MAXFLDNAMESZ 128 /* e.g.,update table_name set boundary = '( */

extern char *map_string;
extern char *key_string;
extern char *table_string;
extern char *vtype_string;
extern int total_vects;
extern int total_vertices;
extern int total_import;
extern int verbose;
extern int to_postgis;

int getAllOpts (int, char **);
int getSelectOpts (int, char **);
int bin_search (int *, int , int *, int *);
int buildPg(char *,char *,char *,int ,int);
int runPg(char *, char *,char *, int, int);
void build_lookup_tables (struct Map_info *);
int plotCat (char *, char *, struct line_pnts *, int, struct Map_info *, int, PGconn *);
