#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include "local_proto.h"

int main (int argc, char **argv)
{
	int achar ;
	char buf[200];
	char buf2[200];
	char *bptr ;
	int map[1024];
	long newmap[1024];
	int nmap;
	long nchars ;
	long newchars ;
	long offset;
	long newoffset;
	long *index;
	int size;
	FILE *fn ;

	int font;
	int newfont ;

	if (argc != 2)
	{
		fprintf (stdout,"USAGE: splitfont directory\n") ;
		exit(-1) ;
	}

	font = 0; /* stdin */

	read (font, &offset, sizeof(offset));
	lseek (font, offset, 0);

	read (font, &nchars, sizeof nchars);
	size = nchars * sizeof (*index);
	index = (long *)malloc (size);
	if (read (font, index, size) != size)
	{
		fprintf (stdout,"can't read index!\n");
		exit(1);
	}


	fn = popen ("cd ../fonts; ls *.hmp | sed 's/.hmp//'", "r");
	while (1)
	{
		if (NULL == fgets(buf,80,fn))
		{
			pclose(fn) ;
			break;
		}

		/* strip off new line */
		bptr = buf ;
		while(*bptr != '\0') bptr++ ;
		*(--bptr) = '\0' ;

		memset(map, '\0', sizeof(map));
		nmap = fontmap (buf, map);
		if (nmap < 0)
		{
			fprintf (stdout,"%s: error\n", buf);
			continue;
		}
		if (nmap == 0)
		{
			fprintf (stdout,"%s: no chars\n", buf);
			continue;
		}

		sprintf(buf2,"%s/%s", argv[1], buf) ;
		newfont = creat (buf2, 0644);
		if (newfont < 0)
		{
			perror ("binfont");
			exit(1);
		}
		/* save space for font index */
		newoffset = 0 ;
		write(newfont, &newoffset, sizeof newoffset) ;

		newchars = 0 ;
		
		/*new files containing description of additional upper 8-bit characters*/
		
		if (nmap>96) {
		    for(achar=-0200; achar<1; (achar)++)
		    {
			offset = index [map[achar + 224]];
			if (offset <= 0)
				fprintf (stdout,"character <%c> (n=%d) not defined in font <%s>\n", 
				achar, (int) achar, buf);
			else
			{
				newchars ++ ;
				lseek (font, offset, 0);
				newmap[achar + 224] = lseek(newfont, 0L, 1) ;
				/*showchar (font);*/
				savechar (font, newfont);
			}
		    }
		}
		/* original 96 symbols files*/
		for(achar=040; achar<0176; (achar)++)
		{
			offset = index [map[achar - 040]];
			if (offset <= 0)
				fprintf (stdout,"character <%c> (n=%d) not defined in font <%s>\n", 
				achar, (int) achar, buf);
			else
			{
				newchars ++ ;
				lseek (font, offset, 0);
				newmap[achar-040] = lseek(newfont, 0L, 1) ;
				/*showchar (font);*/
				savechar (font, newfont);
			}
		}
		newoffset = lseek (newfont, 0L, 1);
		write (newfont, &newchars, sizeof(newchars));
		write (newfont, newmap, newchars * sizeof (*newmap));
		lseek (newfont, 0L, 0);
		write (newfont, &newoffset, sizeof(newoffset));
		close (newfont);
	}
	return 0;
}

int savechar (int font, int newfont)
{
    int n;
    unsigned char X[256], Y[256];


    read  (   font, &n, sizeof n);
    write (newfont, &n, sizeof n);
    read  (   font, X, n);
    write (newfont, X, n);
    read  (   font, Y, n);
    write (newfont, Y, n);

    return 0;
}
