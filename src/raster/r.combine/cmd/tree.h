#include "gis.h"

#define NUL_STR "\0"
#define NUL_EXP  0

#define MAP_EXPR   1
#define HEAD_EXPR  2
#define STAT_EXPR  3
#define CATS_EXPR  4
#define ERR_EXPR   5
#define WIN_EXPR   6
#define EXIT_EXPR  7
#define HELP_EXPR  8
#define ERAS_EXPR  9
#define HIST_EXPR  10

#define NUL_OPR  1
#define AND_OPR  2
#define  OR_OPR  3
#define NOT_OPR  4
#define GRP_OPR  5
#define NAM_OPR  6
#define LEAF_OPR 7
#define COV_OPR  8
#define OVR_OPR  10
#define OV1_OPR  11
#define OV2_OPR  12
#define OV3_OPR  13
#define OV4_OPR  14

struct Group
{
    char *table          ;    /* table to hold group marks  */
    int min, max         ;    /* min and max number in group */
};

struct Node
{
    char         name[30] ;    /* name of the  map                  */
    char         *mapset  ;    /* mapset  of the map                */

    int          cellfd   ;    /* cell file descriptor              */

    struct Group group    ;    /* For group operator                */
    CELL *cbuf            ;    /* buffer to hold a line of map      */
    int          oper     ;    /* operator code                     */
    struct Node *left     ;    /* left child                        */ 
    struct Node *rite     ;    /* rite child                        */ 
    int          ltot     ;    /* number of left decendants         */
    int          rtot     ;    /* number of rite decendants         */
    int          lscore   ;    /* high score means dont go that way */
    int          rscore   ;
    int          new_cat  ;    /* new category number for cover     */
} ;
