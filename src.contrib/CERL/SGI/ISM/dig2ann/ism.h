
extern char *ISM_Colors[];
extern char *ISM_YN_Strings[];

extern char *ISM_Line_Styles[];
extern char *ISM_Line_Thick[];

extern char *ISM_Area_Patterns[];
extern char *ISM_Area_Size[];

extern char *ISM_Site_Symbols[];
extern char *ISM_Site_Size[];

struct lineray {
    int att;
    int flags;
};

/* byte orderings */
#define LINE_COLOR  0
#define LINE_STYLE  1
#define LINE_WEIGHT 2
#define LINE_YN     3

#define AREA_COLOR   0
#define AREA_PATTERN 1
#define AREA_SIZE    2
#define AREA_YN      3

#define SITE_COLOR   0
#define SITE_SYMBOL 1
#define SITE_SIZE    2
#define SITE_YN      3

#define MAX_YN            2

#define MAX_LINE_STYLE   10
#define MAX_LINE_COLOR    8
#define MAX_LINE_WEIGHT   3

#define MAX_AREA_PATTERN 21
#define MAX_AREA_COLOR    8
#define MAX_AREA_SIZE     9

#define MAX_SITE_SYMBOL  33
#define MAX_SITE_COLOR    8
#define MAX_SITE_SIZE     6

#define L_ATT_POS      1
#define L_CAT_POS      2.5
#define L_COL_POS      5.4
#define L_STYLE_POS    6.9
#define L_WEIGHT_POS  11.4
#define L_YN_POS      12.9

int intcmp ();

extern struct Categories Cats;
extern int User_Entry;

#define CAT_LENGTH 13
