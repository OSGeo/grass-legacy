/* Program: DOS.list     Author: Paul W. Carlson 	4/1/88
 *
 * This program lists the saved GRASS displays in the current MS-DOS
 * directory that were created with the DOS.save program.
 */
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>

main()
{
	char fname[20], c;
	int fd;

	strcpy(fname, G__getenv("DEBTTY"));
	if (-1 == access(fname, 00))
	{	printf("\nSorry, you must be the user that started the monitor.\n");
		exit(-1);
	}
	if ((fd = open(fname, O_RDWR)) == -1)
	{	printf("Can't open %s for reading and writing.\n", fname);
		exit(-1);
	}
	write(fd, "t", 1);
	printf("\n----------------------------------------");
	printf("---------------------------------------");
	printf("\nSaved GRASS displays available:\n\n");
	while (1)
	{	read(fd, &c, 1);
		if (c == ';') break;
		putchar(tolower(c));
	}
	read(fd, &c, 1);
	printf("\n----------------------------------------");
	printf("---------------------------------------\n\n");
	close(fd);
}
