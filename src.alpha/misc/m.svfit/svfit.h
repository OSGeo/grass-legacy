#ifndef lint
static char *version= "0.4B <20 Oct 1994>";
#endif

#define PLOTPROG "g.gnuplot"

#define PMAX 3 /* maximum number of parameters to models */

/* for when using the dumb terminal of gnuplot. */
#define RETURN "                              HIT <RETURN> TO CONTINUE"
/* To use dumb terminal, uncomment the following line */
/* #define DUMB */


/* semivariogram models */
#define NMODELS 10
#define LINEAR 1
#define SPHERICAL 2
#define EXPONENTIAL 3
#define GAUSSIAN 4
#define QUADRATIC 5 
#define HOLE_EFFECT 6 
#define POWER 7
#define NUGGET 8
#define RBF 9

#ifndef lint
static char *modelnames[NMODELS] = { 
        "(undefined)",
        "Linear",
        "Spherical",
        "Exponential",
        "Gaussian",
        "Quadratic",
        "Wave",
        "Power",
        "Nugget" ,
        "RBF of s.surf.2d"};
#endif

/* semivariogram estimators */
#define NESTIMATORS	11
#define CLASSICAL 	1
#define MEAN4ROOT 	2
#define MEDIAN4ROOT 	3
#define MADOGRAM 	4
#define RODOGRAM 	5
#define GENERAL_REL 	6
#define PAIRWISE_REL 	7
#define COVARIANCE 	8
#define CORRELOGRAM 	9

#ifndef lint
static char *estimatornames[NESTIMATORS] = { 
 "(undefined)",
 "Classical",
 "Mean Fourth Root",
 "Median Fourth Root",
 "Semimadogram",
 "Semirodogram",
 "General Relative",
 "Pairwise Relative",
 "Covariance",
 "Correlogram" };
#endif

/* semivariance struct, aka "working list" */
struct semivar_list
{
  int n;                        /* # pairs used to compute */ 
  double g, h;                  /* semivariance at this lag */
};
typedef struct semivar_list HGN;

/* parameter structure, to store semivariogram parameters */
struct pstruct
{
  int estimator;
  int model;
  int omni;
  double range;
  double sill;
  double c0,c1,c2;
  double dist_interval;  /* lag distance */
  double distance_tol;   /* tolerance on lag */
  double direction;      /* angle for directional variogram */
  double angle_tol;      /* angular tolerance */
};
typedef struct pstruct PARAM;

char **cdpath ();
void save_plot ();
PARAM pre_model();
void update_parameters ();
void display_parameters ();
double *solvex ();
char *hgnwrite ();
