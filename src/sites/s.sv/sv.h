/* z structure, aka "sites list" */
struct zstruct
{
  double x, y, z;
};
typedef struct zstruct Z;


/* semivariance struct, aka "working list" */
struct semivar_list
{
  int n;                        /* # pairs used to compute */ 
  double g, h;                  /* semivariance at this lag */
};
typedef struct semivar_list HGN;

#define PLOTPROG "g.gnuplot"
#define POINTSSTYLE "1 8"
#define LINESTYLE "2 8"

/* angleok.c */
int angle_ok(double, double, double);
/* cdpath.c */
char **cdpath(char *, int *, char *, char *);
/* nbins.c */
int nbins(double);
/* plthgpt.c */
int plot_hg_points(HGN *, int, int, char *);
/* save.c */
void save_plot(char *, char *);
