/* @(#)mapdef.h	AMG v.3.2 */
# ifdef MAIN
# define EXT
# else
# define EXT extern
# endif

# define MAXLINE 1000

EXT int verbose, prompt;
EXT int	mapdef;
EXT char *def_name;
EXT char line[MAXLINE];
EXT char *f_tempa;
EXT char *f_tempb;
EXT struct map_def def;
EXT double cpc;

# define DEF_CTS_CM 400.  /* default plotter resolution */

# define PROJ_PROG	"proj"
# define APPROX		"bivar"

double dmstor();
	/* basic path of temporary work area */
# define TEMP "/tmp"
