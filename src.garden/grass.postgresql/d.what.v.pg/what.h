#include "gis.h"
#include "vect/digit.h"

extern int fd;
extern int dbCat;
extern int h_num;
extern char *name;
extern char *mapset;

int getAllOpts (int, char **);
int getSelectOpts (int, char **);
int buildInfxQry(struct Option *,struct Option *, int);
int runInfxQry(char *, char *);
int getCat (struct Map_info *, struct Categories *, int, int, int);
char* openvect (char *);
