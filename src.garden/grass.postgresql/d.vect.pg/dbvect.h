#include "vect/digit.h"

#define QRY_LENGTH 1024

extern int extract_yes;

int getAllOpts (int, char **);
int getSelectOpts (int, char **);
int bin_search (int *, int , int *, int *);
int buildPg(char *,char *,char *,char *,char *,int ,int);
int runPg(char *, char *,char *, int, int);
void build_lookup_tables (struct Map_info *);
int plotCat (char *, char *, struct line_pnts *, int, struct Map_info *, int);
