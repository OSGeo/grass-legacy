#include "gis.h"
#include "vect/digit.h"

#define QRY_LENGTH 1024

extern int fd;
extern int dbCat;
extern int h_num;
extern char *name;
extern char *mapset;

int getAllOpts (int, char **);
int getSelectOpts (int, char **);
int buildPg(struct Option *,struct Option *, int, char *);
int runPg(char *, char *);
int getCat (struct Map_info *, struct Categories *, int, int, int);
char* openvect (char *);
