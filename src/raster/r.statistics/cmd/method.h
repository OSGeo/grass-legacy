#define DISTRIB 0
#define AVERAGE 1
#define MODE    2
#define MEDIAN  3
#define ADEV    4       /* Average deviation     */
#define SDEV    5       /* Standard deviation    */
#define VARIANC 6       /* Variance              */
#define SKEWNES 7       /* Skewnes               */
#define KURTOSI 8       /* Kurtosis              */
#define MIN	9	/* Minimum		 */
#define MAX	10	/* Maximum		 */


struct menu
{
    char *name;  	/* method name */
    int  val;           /* number of function */
    char *text;		/* menu display - full description */
};



#ifdef MAIN

/* modify this table to add new methods */
    struct menu menu[] = {
    "distribution", DISTRIB, "distribution of values in specified objects in %%",
    "average",      AVERAGE, "average of values in specified objects",
    "mode",         MODE,    "mode of values in specified objects",
    "median",       MEDIAN,  "median of values in specified objects",
    "avedev",       ADEV,    "Average deviation of values in specified objects",
    "stddev",       SDEV,    "Standard deviation of values in specified objects",
    "variance",     VARIANC, "Variance of values in specified objects",
    "skewness",     SKEWNES, "Skewnes of values in specified objects",
    "kurtosis",     KURTOSI, "Kurtosis of values in specified objects",
    "min",	    MIN,     "Minimum of values in specified objects",
    "max",	    MAX,     "Maximum of values in specified objects",
    0,0,0 };

#else
    extern struct menu menu[];
#endif
