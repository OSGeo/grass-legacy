#include <stdio.h>
#include <stdlib.h>


#define	DBYTES		8
#define	BUFFER_SIZE	256


int
main(int argc, char **argv)
{
	double	**data, smallest;
	char	tofile[BUFFER_SIZE], file[BUFFER_SIZE], system[BUFFER_SIZE];
	int	nl, ns, i, i1, i2, i3, itemp, j;
	FILE	*infile, *tmpfp;

#if 0
	printf("ENTER NL,NS,TOPO FILE,SYSTEM(UNIX,VMS,PRIME)\n");
#endif
	scanf("%s", file);
	if(!(tmpfp = fopen(file, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", file);
		exit(1);
	}

	fscanf(tmpfp, "%d %d", &nl, &ns);
	fscanf(tmpfp, "%s", tofile);
	fscanf(tmpfp, "%s", system);
	fclose(tmpfp);

	if(!(infile = fopen(tofile, "r+"))){
		fprintf(stderr, "%s: No such file or open failed\n", tofile);
		exit(1);
	}

	data = (double **)malloc(ns*sizeof(double *)) - 1;
	for(i=1; i<=ns; i++)
		data[i] = (double *)malloc(3*sizeof(double)) - 1;

	i1 = 1;
	i2 = 2;
	i3 = 3;
	for(j=1; j<=ns; j++)
		fread(&data[j][1], DBYTES, 1, infile);
	for(j=1; j<=ns; j++)
		fread(&data[j][2], DBYTES, 1, infile);
	for(j=1; j<=ns; j++)
		fread(&data[j][3], DBYTES, 1, infile);

	for(i=2; i<=nl-1; i++){
		for(j=2; j<=ns-1; j++){
			smallest = data[j-1][i1];
			if(data[j][i1] < smallest)
				smallest = data[j][i1];
			if(data[j+1][i1] < smallest)
				smallest = data[j+1][i1];
			if(data[j-1][i2] < smallest)
				smallest = data[j-1][i2];
			if(data[j+1][i2] < smallest)
				smallest = data[j+1][i2];
			if(data[j-1][i3] < smallest)
				smallest = data[j-1][i3];
			if(data[j][i3] < smallest)
				smallest = data[j][i3];
			if(data[j+1][i3] < smallest)
				smallest = data[j+1][i3];
			if(data[j][i2] < smallest)
				data[j][i2] = smallest;
		}
		fseek(infile, (i-1)*ns*DBYTES, SEEK_SET);
		for(j=1; j<=ns; j++)
			fwrite(&data[j][i2], DBYTES, 1, infile);
		itemp = i1;
		i1 = i2;
		i2 = i3;
		i3 = itemp;
		if(i == nl-1)
			continue;
		fseek(infile, (i+1)*ns*DBYTES, SEEK_SET);
		for(j=1; j<=ns; j++)
			fread(&data[j][i3], DBYTES, 1, infile);
	}

	fclose(infile);

	for(i=1; i<=ns; i++)
		free(data[i] + 1);
	free(data + 1);

	exit(0);
}

