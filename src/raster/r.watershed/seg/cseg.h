#include "segment.h"

#define CSEG struct _c_s_e_g_
CSEG
{
	SEGMENT	seg;         /* segment structure */
	int	fd;          /* fd for reading/writing segment file */
	char	*filename;   /* name of segment file */
	char	*name;       /* cell file read into segment file */
	char	*mapset;
} ;

#define DSEG struct _d_s_e_g_
DSEG
{
	SEGMENT	seg;         /* segment structure */
	int	fd;          /* fd for reading/writing segment file */
	char	*filename;   /* name of segment file */
	char	*name;       /* cell file read into segment file */
	char	*mapset;
} ;

#define BSEG struct _b_s_e_g_
BSEG
{
	SEGMENT	seg;         /* segment structure */
	int	fd;          /* fd for reading/writing segment file */
	char	*filename;   /* name of segment file */
	char	*name;       /* cell file read into segment file */
	char	*mapset;
} ;

#define SSEG struct _s_s_e_g_
SSEG
{
	SEGMENT	seg;         /* segment structure */
	int	fd;          /* fd for reading/writing segment file */
	char	*filename;   /* name of segment file */
} ;
