#include <stdio.h>
process_old (in, out, file)
    FILE *in, *out;
    char *file;
{
    if (!in)
	return 1;

    while (gather(in))
    {
	setup (file,0);
	if(!modify())
	    return 0;
	update(out);
    }
    return 1;
}
