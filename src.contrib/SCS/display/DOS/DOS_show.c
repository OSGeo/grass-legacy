/* Program: DOS.show     Author: Paul W. Carlson 	8/88
 *
 * The programs displays a saved GRASS file from the MS-DOS hard disk.
 */

#include <stdio.h>
#include <fcntl.h>
#include "gis.h"

main(argc, argv)
int argc;
char *argv[];
{
	int leng, n;
	char buf[80], file_name[10];
	char *p, fname[20];
	int fd;

	G_gisinit (argv[0]);

	printf(" Enter a file name : ");
	gets(file_name);
	leng = strlen(file_name);
	if (leng == 0) exit(0);
	p = file_name;
	for (n = 0; n < leng; n++)
	{	if (*p == '.')
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

	write(fd, "s", 1);
	buf[0] = (leng & 0x1f) | 0x20;
	buf[1] = ((leng & 0x03e0) >> 5) | 0x20;
	write(fd, buf, 2);
	write(fd, file_name, leng);

	/* get return code */

	read(fd, buf, 2);
	switch (*buf)
	{  case 'n':
	   {	printf( "\nReading MS-DOS file \"%s\"...\n", file_name);
		fflush(stdout);
		break;
	   }
	   case 'e':
	   {	printf("\nMS-DOS can't open \"%s\"\n", file_name);
		close(fd);
		exit(-1);
	   }
	   default:
	   {	printf( "\nUnknown status, %s  returned from MS-DOS.\n");
		close(fd);
		exit(-1);
	   }
	}

	/* get the completion code */

	read(fd, buf, 2);
	if (*buf == 'c') 
		printf("\nDisplay restore from MS-DOS file \"%s\" is complete.\n", 
		file_name);
	else 
	   printf("\nUnknown status returned from MS-DOS.\n");
	close(fd);
}
