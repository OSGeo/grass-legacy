#include <stdio.h>
#include <ctype.h>

main ()
{
int STATE=0, CNTY=0;
char c[5], FIPSfile[100], buff[200];
FILE *fipsfile;

	sprintf(FIPSfile,"%s/etc/FIPS.code\0",getenv("GISBASE"));

try_agin:

/* open input */
	   if (access(FIPSfile,4) != 0)
		{
		sprintf(buff," Can not open FIPS code file %s\n",FIPSfile) ;
		G_fatal_error(buff) ;
		}
	   fipsfile = fopen (FIPSfile,"r");
		get_fips(&STATE,&CNTY,fipsfile);

fprintf (stdout,"%d %d\n",STATE,CNTY);
}
