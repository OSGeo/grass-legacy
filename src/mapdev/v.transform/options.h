
#include	"gis.h"

#define MAPIN	1
#define MAPOUT	2
#define COORD	3
#define VERBOSE	4

/*  We want main.c to access the following, but not command.c  */

#ifdef  MAIN

	struct Command_keys com_keys[] =
	{
	    {"mapin",  MAPIN},
	    {"in",     MAPIN},
	    {"mapout", MAPOUT},
	    {"out",    MAPOUT},
	    {"coordfile", COORD},
	    {"coord",  COORD},
	    {"coors",  COORD},
	    {"coor",   COORD},
	    {"c",      COORD},
	    {"verbose", VERBOSE},
	    {"v",      VERBOSE},
	    {NULL,     0}
	} ;

#endif

