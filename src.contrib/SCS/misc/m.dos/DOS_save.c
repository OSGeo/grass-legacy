/* Program: DOS.save     Author: Paul W. Carlson 	8/88
 *
 * The programs saves a GRASS display to the MS-DOS hard disk in the
 * current MS-DOS directory.  The emulator adds the extension .GRS to
 * the filename.
 */
#include <stdio.h>
#include <fcntl.h>
main(argc, argv)
int argc;
char *argv[];
{
	int leng, n;
	char buf[8];
	char fname[20];
	int fd;

	if (argc !=2) 
	{	printf("\nUsage: DOS.save <filename>\n\n");
		 exit(-1);
	}
	leng = strlen(argv[1]);
	for (n = 0; n < leng; n++)
	{	if (argv[1][n] == '.')
		{	printf( "\nNo extension allowed in filename\n\n");
		 	exit(-1);
		}
	}
	if (leng > 8)
	{	printf( "\nMaximum of 8 characters allowed in filename\n\n");
		 exit(-1);
	}

	strcpy(fname, G__getenv("DEBTTY"));
	if (-1 == access(fname, 00))
	{	printf("\nSorry, you must be the user that started the monitor.\n");
		exit(-1);
	}
	if ((fd = open(fname, O_RDWR)) == -1)
	{	printf("\nCan't open %s for reading and writing.\n", fname);
		exit(-1);
	}

	/* send the emulator command identifier and file name */

	write(fd, "S", 1);
	buf[0] = (leng & 0x1f) | 0x20;
	buf[1] = ((leng & 0x03e0) >> 5) | 0x20;
	write(fd, buf, 2);
	write(fd, argv[1], leng);

	/* get return code */

	read(fd, buf, 2);
	switch (*buf)
	{  case 'n':
	   {	printf( "\nWriting MS-DOS file \"%s\"...\n", argv[1]);
		fflush(stdout);
		break;
	   }
	   case 'e':
	   {	printf("\nMS-DOS can't open \"%s\"\n", argv[1]);
		close(fd);
		exit(-1);
	   }
	   case 'a':
	   {	printf("\nInsufficient MS-DOS disk space\n");
		close(fd);
		exit(-1);
	   }
	   default:
	   {	printf( "\nUnknown status returned from MS-DOS.\n");
		close(fd);
		exit(-1);
	   }
	}

	/* get the completion code */

	read(fd, buf, 2);
	if (*buf == 'c') 
		printf("\nDisplay save to MS-DOS file \"%s\" is complete.\n", 
		argv[1]);
	else 
	   printf("\nUnknown status returned from MS-DOS.\n");
	close(fd);
}
