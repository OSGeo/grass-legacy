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
#define SUM	11	/* Sum  		 */

struct stats
{
    int nalloc;
    int n;
    long *cat;
    double *area;
};

struct menu
{
    char *name;  	/* method name */
    int  val;           /* number of function */
    char *text;		/* menu display - full description */
};



#ifdef MAIN

/* modify this table to add new methods */
    struct menu menu[] = {
    {"distribution", DISTRIB, "distribution of values in specified objects in %%"},
    {"average",      AVERAGE, "average of values in specified objects"},
    {"mode",         MODE,    "mode of values in specified objects"},
    {"median",       MEDIAN,  "median of values in specified objects"},
    {"avedev",       ADEV,    "Average deviation of values in specified objects"},
    {"stddev",       SDEV,    "Standard deviation of values in specified objects"},
    {"variance",     VARIANC, "Variance of values in specified objects"},
    {"skewness",     SKEWNES, "Skewnes of values in specified objects"},
    {"kurtosis",     KURTOSI, "Kurtosis of values in specified objects"},
    {"min",	    MIN,     "Minimum of values in specified objects"},
    {"max",	    MAX,     "Maximum of values in specified objects"},
    {"sum",	    SUM,     "Sum of values in specified objects"},    
    {0,0,0} };

#else
    extern struct menu menu[];
#endif

/* main.c */
int is_ok(char *, char *);
/* median.c */
long median(struct stats *);
/* moment.c */
int stats(double *, int, double *, double *, double *, double *, double *, double *);
/* o_adev.c */
int o_adev(char *, char *, char *, int, struct Categories *);
int a_dev(double *, int, double *);
/* o_average.c */
int o_average(char *, char *, char *, int, struct Categories *);
int out(FILE *, long, double, double);
/* o_distrib.c */
int o_distrib(char *, char *, char *, int);
int o_out(FILE *, long, long);
/* o_kurt.c */
int o_kurt(char *, char *, char *, int, struct Categories *);
int kurt(double *, int, double *);
/* o_max.c */
int o_max(char *, char *, char *, int, struct Categories *);
/* o_median.c */
int o_median(char *, char *, char *, int, struct Categories *);
/* long median(struct stats *); Prototyped in median.c */
/* o_min.c */
int o_min(char *, char *, char *, int, struct Categories *);
/* o_mode.c */
int o_mode(char *, char *, char *, int, struct Categories *);
/* o_sdev.c */
int o_sdev(char *, char *, char *, int, struct Categories *);
int s_dev(double *, int, double *);
/* o_skew.c */
int o_skew(char *, char *, char *, int, struct Categories *);
int skew(double *, int, double *);
/* o_sum.c */
int o_sum(char *, char *, char *, int, struct Categories *);
int sum_out(FILE *, long, double);
/* o_var.c */
int o_var(char *, char *, char *, int, struct Categories *);
int m_var(double *, int, double *);
/* read_stats.c */
int read_stats(FILE *, long *, long *, double *);
/* skew.c */
int kurt(double *, int, double *);
int skew(double *, int, double *);
/* write_rec.c */
int write_reclass(FILE *, long, long, char *, int);
