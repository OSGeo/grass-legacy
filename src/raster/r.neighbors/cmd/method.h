typedef int (*ifunc)();
typedef CELL (*cfunc)();
struct menu
{
    cfunc method;	/* routine to compute new value */
    ifunc cat_names;	/* routine to make category names */
    int copycolr;	/* flag if color table can be copied */
    char *name;  	/* method name */
    char *text;		/* menu display - full description */
};
#ifdef MAIN
#define NO_CATS 0


/* declare all methods here */
    CELL c_median();
    CELL c_mode();
    CELL c_min();
    CELL c_max();
    CELL c_ave();
    CELL c_stddev();
    CELL c_var();
    CELL c_divr();
    CELL c_intr();

/* declare all category name routines here */
/* note: these routines must not read from the tty */
    int null_cats();
    int divr_cats();
    int intr_cats();

/* modify this table to add new methods */
    struct menu menu[] = {

    c_ave,    NO_CATS, 1,   "average", "average value",
    c_median, NO_CATS, 1,   "median","median value",
    c_mode,   NO_CATS, 1,   "mode",  "most frequently occuring value",
    c_min,    NO_CATS, 1,   "minimum", "lowest value",
    c_max,    NO_CATS, 1,   "maximum", "highest value",
    c_stddev, NO_CATS, 0,   "stddev", "standard deviation",
    c_var,    NO_CATS, 0,   "variance", "statistical variance",
    c_divr,   divr_cats, 0, "diversity", "number of different values",
    c_intr,   intr_cats, 0, "interspersion", "number of values different than center value",

    0,0,0,0,0 };

#else
    extern struct menu menu[];
#endif
