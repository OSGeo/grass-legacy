#ifndef lint
static char *SCCSID = "@(#)loaddef.c	AMG v.3.1";
#endif
/* load map definition file */
# include <fcntl.h>
# include <stdio.h>
# include "mapgen.h"

extern struct map_def def;

loaddef(name)
char *name;
{
	int f, ret;

	if ((f = open(name, O_RDONLY)) < 0)
		emess(2,"can't open map defintion file:",name);

	ret = 0;
	if (read(f, &def, sizeof(def) + 1) != sizeof(def))
		emess(1,"mapdef load lenth error",(char *)0);

	if (def.magic != MAGIC)
		emess(1,"MAGIC check error",(char *)0);

	close(f);
	return (ret);
}
