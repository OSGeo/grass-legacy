typedef int (*ifunc)();
typedef CELL (*cfunc)();
struct menu
{
    cfunc method;	/* routine to compute new value */
    ifunc cat_names;	/* routine to make category names */
    int copycolr;	/* flag if color table can be copied */
    char *title;	/* menu display - short title */
    char *text;		/* menu display - full description */
};
#ifdef MAIN
#define NO_CATS 0


/* declare all methods here */
    CELL c_mode();
    CELL c_min();
    CELL c_max();
    CELL c_ave();
    CELL c_divr();
    CELL c_intr();

/* declare all category name routines here */
/* note: these routines must not read from the tty */
    int null_cats();
    int divr_cats();
    int intr_cats();

/* modify this table to add new methods */
    struct menu menu[] = {

    c_mode, NO_CATS, 1,   "mode","most frequently occuring category",
    c_min,  NO_CATS, 1,   "min", "lowest category",
    c_max,  NO_CATS, 1,   "max", "highest category",
    c_ave,  NO_CATS, 1,   "ave", "average category",
    c_divr, divr_cats, 0, "diversity", "number of different categories",
    c_intr, intr_cats, 0, "interspersion", "number of categories different than center category",

    0,0,0,0,0 };

#else
    extern struct menu menu[];
#endif
