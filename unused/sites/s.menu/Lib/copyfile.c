#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

int copyfile (char *filea, char *fileb)
{
	int fa;
	int fb;
	char buf[1024];
	int n;

	if ((fa = open (filea, 0)) < 0)
	{
		perror (filea);
		return -1;
	}
	if ((fb = creat (fileb, 0666)) < 0)
	{
		close (fa);
		perror (fileb);
		return -1;
	}

	while ((n = read (fa, buf, sizeof buf)) > 0)
		write (fb, buf, n);
	
	close (fa);
	close (fb);

	return 0;
}
