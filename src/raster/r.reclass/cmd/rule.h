#include "gis.h"

#define RULE struct _rule_
RULE
{
    CELL new;
    CELL lo;
    CELL hi;
    RULE *next;
};
/* add_rule.c */
int add_rule(RULE **, CELL, CELL, CELL);
/* parse.c */
int parse(char *, RULE **, RULE **, struct Categories *);
/* reclass.c */
int reclass(char *, char *, char *, RULE *, struct Categories *, char *);
int _reclass(RULE *, struct Categories *, struct Reclass *);
int re_reclass(RULE *, struct Categories *, struct Reclass *, struct Reclass *, char *, char *);
/* input.c */
int input(char *);
/* range.c */
int new_range(char *, struct Reclass *);
/* stats.c */
int new_stats(char *, struct Reclass *);
