#include <stdio.h>
#include "glob.h"
#include "local_proto.h"

int output (char *buf)
{
	if (lineno >= pagelen)
		top_of_page();
	
	fprintf (stdout,"%s\n", buf);
	lineno++;

	return 0;
}
