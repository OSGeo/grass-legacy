/* %W%   %G% */
#include <stdio.h>
main()
{
	int tmp ;
	char buf[60];

	sprintf(buf,"%s/fonts\0",getenv("GISBASE"));
	fprintf(stderr,"FONTS = %s\n",buf);
}
