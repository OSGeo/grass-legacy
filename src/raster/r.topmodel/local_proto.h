#include <stdio.h>
#include <math.h>
#include <time.h>
#include "gis.h"


#define	NONE		0
#define	BKWD		1
#define	FRWD		2
#define	AVG		NONE

#define	FILL		0x1
#define	DIR		0x2
#define	BELEV		0x4
#define	TOPIDX		0x8
#define	IDXSTATS	0x10
#define	OUTPUT		0x20

#define	BUFSIZE		1024
#define	ZERO		0.0000001
#define	TOLERANCE	0.001
#define	MAXITER		100
#define	NTERMS		10


/* check_ready.c */
int	check_ready(void);
int	check_required(void);
int	check_names(void);
int	check_io(void);
/* misc.c */
int	run(char *buf);
void	gregion(void);
void	depressionless(void);
void	basin_elevation(void);
void	top_index(void);
/* file_io.c */
void	get_line(FILE *fp, char *buffer);
void	read_inputs(void);
void	write_outputs(void);
/* topmodel.c */
double	get_lambda(void);
void	initialize(void);
void	implement(void);
double	get_Em(void);
void	others(void);
void	topmodel(void);
/* infiltration.c */
double	get_f(double t, double R);


#ifdef MAIN
#	define	GLOBAL
#else
#	define	GLOBAL	extern
#endif


/* Topographic index statistics file */
GLOBAL	struct
{
	/* misc.nidxclass's */
	double	*atb, *Aatb_r;
} idxstats;

/* Parameters file */
GLOBAL	struct
{
	char	*name;
	double	A, qs0, lnTe, m, Sr0, Srmax, td, vch, vr;
	int	infex;
	double	K0, psi, dtheta;
	int	nch;
	/* params.nch's */
	double	*d, *Ad_r;
} params;

/* Input file */
GLOBAL	struct
{
	int	ntimestep;
	double	dt;
	/* input.ntimestep's */
	double	*R_, *Ep_;
} input;

/* Map names */
GLOBAL	struct
{
	char	*elev, *basin, *belev, *fill, *dir, *topidx;
} map;

/* File names */ 
GLOBAL	struct
{
	char	*idxstats, *params, *input, *output, *Qobs;
} file;

/* Miscellaneous TOPMODEL variables */
GLOBAL	struct
{
	/* Number of non-null cells */
	int	ncell;
	/* Number of topographic index classes */
	int	nidxclass;
	/* Model efficiency */
	double	Em;
	/* '_' suffix means time units in time step */
	int	ndelay_, nreach_;
	double	lnTe_, vch_, vr_;
	double	lambda;
	double	_qs_, qs0_;
	double	Qobs_peak_, Qt_peak_, Qobs_bar_, Qt_bar_;
	int	tobs_peak_, tt_peak_;
	/* params.nch's */
	double	*tch_;
	/* misc.nreach_'s */
	double	*Add;
	/* input.ntimestep's */
	double	*Qobs_;
	double	*Qt_;
	double	*qs_;	/* spatially constant? */
	double	*Sbar_;
	double	*f_;
	double	*fex_;
	/* input.ntimestep * (misc.nidxclass + 1)'s */
	double	**qt_, **qo_, **qv_;
	/* input.ntimestep * misc.nidxclass's */
	double	**Srz_, **Suz_;
	double	**S_;
	double	**Ea_;
	double	**ex_;
	/* Miscellaneous variables */
	int	timestep, idxclass;
} misc;


GLOBAL	struct
{
	/* Input flag */
	char	input;
	/* Overwrite flag */
	char	overwr;
	/* Overwrite list */
	int	overwrlist;
	/* Wide flag */
	char	wide;
} flg;


/* Miscellaneous variables */
GLOBAL	char	*gisbase, *mapset;
GLOBAL	char	buf[BUFSIZE];

