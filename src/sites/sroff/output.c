#include "glob.h"

output (buf)

	char *buf;
{
	if (lineno >= pagelen)
		top_of_page();
	
	printf("%s\n", buf);
	lineno++;
}
