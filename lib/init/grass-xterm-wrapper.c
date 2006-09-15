#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
main(int argc, char *argv[])
{
	int i, j, l;
	char *buf, *wd;

	if(!(wd = getenv("WD")))
		exit(EXIT_FAILURE);

	for(i=1; i<argc && strcmp(argv[i], "-e")!=0; i++);
	if(i == argc)
		exit(EXIT_SUCCESS);

	l = strlen(wd)+128;
	for(j=++i; j<argc; j++)
		l += strlen(argv[j])+1;
	buf = (char *)malloc(l);
	sprintf(buf, "%ssh.exe -c '%s", wd, argv[i++]);
	for(; i<argc;)
		sprintf(buf, "%s %s", buf, argv[i++]);
	sprintf(buf, "%s' 2>&1", buf);

	exit(system(buf));
}
