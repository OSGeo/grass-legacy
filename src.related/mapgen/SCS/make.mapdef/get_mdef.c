#include <stdio.h>

main()
{
FILE *fp;
char line[150];
int i;

fp = NULL;
while (fp == NULL) {
	i=0;line[0]=0;
	fprintf(stderr,"Enter the name of the map definition file [map.def]: ");
	while ((line[i] = getchar()) != '\n') i++; 
	line[i] = 0;
	if (line[0] == 0) sprintf(line,"map.def");
	if ((fp = fopen(line,"r")) == NULL)
		fprintf(stderr,"Mapdef file %s not found\n",line);
	}
printf("%s",line);
fclose(fp);
exit(1);
}
