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


int readsites () ;
int readsites2 () ;
int atoi ();
char **cdpath ();
int dblcompare ();
void save_plot ();
void zread ();
