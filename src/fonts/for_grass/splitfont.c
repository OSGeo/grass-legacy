#include <stdio.h>
main(argc, argv)
	char **argv ;
{
	char achar ;
	char buf[200];
	char buf2[200];
	char *bptr ;
	int map[1024];
	int newmap[1024];
	int nmap;
	long nchars ;
	long newchars ;
	long offset;
	long newoffset;
	long *index;
	int n;
	int size;
	FILE *popen() ;
	FILE *fn ;

	int font;
	int newfont ;

	if (argc != 2)
	{
		printf("USAGE: splitfont directory\n") ;
		exit(-1) ;
	}

	font = open ("font.bin", 0);
	if (font < 0)
	{
		perror ("font.bin");
		exit(1);
	}
	read (font, &offset, sizeof(offset));
	printf ("index is at %ld\n", offset);
	lseek (font, offset, 0);

	read (font, &nchars, sizeof nchars);
	printf ("font contains %d characters\n", nchars);
	size = nchars * sizeof (*index);
	index = (long *)malloc (size);
	if (read (font, index, size) != size)
	{
		printf ("can't read index!\n");
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
		while(*bptr != NULL) bptr++ ;
		*(--bptr) = NULL ;

		nmap = fontmap (buf, map);
		if (nmap < 0)
		{
			printf ("%s: error\n", buf);
			continue;
		}
		if (nmap == 0)
		{
			printf ("%s: no chars\n", buf);
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
		write(newfont, newoffset, sizeof newoffset) ;

		newchars = 0 ;
		for(achar=040; achar<0176; (achar)++)
		{
			offset = index [map[achar - 040]];
			if (offset <= 0)
				printf ("character <%c> not defined in font <%s>\n", achar, buf);
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
	exit(0);
}

savechar (font, newfont)
{
    int n;
    unsigned char X[256], Y[256];
    int x,y;


    read  (   font, &n, sizeof n);
    write (newfont, &n, sizeof n);
    read  (   font, X, n);
    write (newfont, X, n);
    read  (   font, Y, n);
    write (newfont, Y, n);
}
