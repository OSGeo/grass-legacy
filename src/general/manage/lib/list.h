#include "gis.h"
struct list
{
    char **element;	/* list of related elements */
    char *alias;	/* element alias */
    char **desc;	/* description of elements */
    char *text;		/* menu text */
    int nelem;		/* number of elements */
    char status;
};

#ifdef MAIN
    int nlist;
    struct list *list;
#endif

    extern int nlist;
    extern struct list *list;

#define REMOVE 1
#define RENAME 2
#define COPY   3
#define LIST   4

char *find();
char *ask_old();
char *ask_new();
char *ask_in_mapset();
