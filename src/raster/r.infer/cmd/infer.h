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

/*
**      External stuff
*/

extern char *progname;
extern int verbose;
extern STR *SP;
extern MAPS *MAP;
extern RULE_T *Rule[];
extern int nrules;
int (*askmap)() ;
int (*do_infer)() ;

/* Prototypes from local sources */
/* compile.c */
int compile(register FILE *);
int newstate(register int);
int push(register int, register STR *);
STR *savestr(int, char *);
int getkey(char *);
int squeeze(register char *, int);
int stripbl(register char *);
int rulealloc(void);
char *emalloc(unsigned);
struct Cell_stats *make_cats(char *);
int mark_cats(struct Cell_stats *, int, int);
int freecats(struct Cell_stats *);
int catmatch(struct Cell_stats *, struct Cell_stats *);
int equal(char *, char *);
/* infer.c */
int main(int, char **);
int infer(void);
int verify(register RULE_T *);
int prove(ELEMENT_T *);
int askmap_map(register ELEMENT_T *);
int askmap_person(register ELEMENT_T *);
int prrule(register RULE_T *, FILE *);
char *prstr(register ELEMENT_T *);
char *prval(register ELEMENT_T *);
char *prtype(int, register char *);
int pushwhy(register RULE_T *);
int popwhy(void);
int showwhy(void);
int prcirc(void);
int clear_truths(void);
/* run_maps.c */
int initialize_map_categories(void);
int run_through_maps(void);
int test_infer(void);
int open_maps(void);
int tell_open_maps(void);
int close_maps(void);
int read_maps(int);
int incr_cols(void);
int record_category_label(int, char *);
