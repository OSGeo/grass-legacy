#include <stdio.h>
#include "gis.h"

int	check_ready(void);
int	adjcellhd(struct Cell_head *cellhd);
void	rdwr_gridatb(void);


#ifdef MAIN
#	define	GLOBAL
#else
#	define	GLOBAL	extern
#endif

GLOBAL	struct	Cell_head	cellhd;
GLOBAL	FCELL	*cell;
GLOBAL	char	*file;
GLOBAL	char	*mapset,*iname;
GLOBAL	char	overwr;
GLOBAL  char    *oname;
