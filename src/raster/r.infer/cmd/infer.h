#include "gis.h"
typedef struct MAPS {  /* GRASS maps used in analysis */
    char *p;               /* pointer to map name */
    short val;             /* Map number */
    int fd;                /* Map file descriptor */
    CELL *rbuf;            /* pointer to map row buffer */
    CELL *cptr;            /* pointer to current col in buffer */
    struct MAPS *next;     /* Next map */
} MAPS ;

typedef struct STR {        /* Keeper of the Strings */
    char    *p;                 /* pointer to map name */
    MAPS    *map;               /* Associated map (if any) */
    struct Cell_stats  *cats;   /* pointer to category stats */
    CELL    hypval;             /* To hold maphyp value; new category num */
    short   val;                /* UNKNOWN, TRUE, FALSE */
    struct  STR *next;          /* yet-another-linked-list */
} STR;

typedef struct ELM {        /* Antecedent or Consequence */
    STR     *str;               /* Associated string */
    short   type;               /* ISTRING or MAPOP, NOT, HYP */
    struct  ELM *next;          /* kept in linked-list */
} ELEMENT_T;

typedef struct {            /* Rule */
    ELEMENT_T   *ant;           /* Head of antecedents for this rule */
    ELEMENT_T   *con;           /* Head of consequences for this rule */
    short       vfy;            /* to detect circular logic */
} RULE_T;

/*
**  General Manifest Constants
*/

# define ANT            1           /* in IF part of rule */
# define CON            2           /* in THEN part of rule */
# define COMMENT_CHAR   '!'         /* ignore lines beginning with this */
# define MAXRULE        1000        /* plenty */
# define MAXWHY         100         /* things proven true, plus current */
# define STRSIZE        512         /* should be plenty */


/*
**      Type definitions
*/

# define ISTRING     000             /* display this string */
# define MAPOP      001             /* run this string via the shell */
# define NOT        002             /* invert truth-value of assoc. string */
# define HYP        004             /* if TRUE, exit */
# define COMMENT    010             /* this line is a comment */
# define NONE       020             /* no keyword recognized */

/*
**  Defines for element vals
*/

# define UNKNOWN    42          /* the answer to Life, the Universe, etc */
# define TRUE       (-1)        /* all binary ones (most places) */
# define FALSE      0           /* all zeros (most places) */

/*
**      External stuff
*/

extern char *progname;
extern int errno;
extern int verbose;
extern STR *SP;
extern MAPS *MAP;
extern RULE_T *Rule[];
extern int nrules;
extern char *emalloc (), *strcpy (), *strcat ();
extern void exit ();
int (*askmap)() ;
int (*do_infer)() ;
