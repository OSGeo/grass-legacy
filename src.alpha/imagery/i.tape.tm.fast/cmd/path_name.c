#include "tape.h"
/********************************************************/
/* NAME:	test_pathname				*/
/*							*/
/* FUNCTION:	examine the name of the device/file that*/
/*		user want to use			*/
/*							*/
/* USAGE:	test_pathname(name)			*/
/*							*/
/* INPUT:	"*name" as string of device/file name	*/
/*							*/
/* OUTPUT:	1 -- device available			*/
/*		0 -- device unavailable			*/
/********************************************************/
test_pathname (name)
    char *name;
{
	if (access (name, 4) == 0)
	  return 1;
	fprintf (stderr, "%s - ",name);
	if (access (name, 0) != 0)
	  fprintf (stderr, "no such device\n");
	else
	  fprintf (stderr, "read permission denied\n");
	return 0;
}
