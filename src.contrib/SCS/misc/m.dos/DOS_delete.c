/* Program: DOS.delete     Author: Paul W. Carlson 	8/88
 *
 * The programs deletes a saved GRASS file from the MS-DOS hard disk.
 */

#include <stdio.h>
#include <fcntl.h>

main(argc, argv)
int argc;
char *argv[];
{
	int leng, n;
	char buf[80];
	char fname[20];
	int fd;

	if (argc !=2) 
	{	printf("\nUsage: DOS.delete <filename>\n\n");
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

	write(fd, "z", 1);
	buf[0] = (leng & 0x1f) | 0x20;
	buf[1] = ((leng & 0x03e0) >> 5) | 0x20;
	write(fd, buf, 2);
	write(fd, argv[1], leng);

	/* get return code */

	read(fd, buf, 2);
	switch (*buf) 
	{   case 'c':
		printf("\nMS-DOS file \"%s\" is deleted.\n", argv[1]);
		break;
	    case 'e':
  		printf("\nMS-DOS file \"%s\" is NOT deleted.\n", argv[1]);
		break;
	    default:
		printf("\nUnknown return code from MS-DOS.\n");
		break;
	}
	close(fd);
}
