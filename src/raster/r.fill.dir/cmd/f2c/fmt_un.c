#include <stdio.h>

#define	DBYTES	8

int
main(int argc, char **argv)
{
	double	i[20000];
	char	tfile[160], t1[160], t2[160], t3[160];
	int	nl,ns,m,m2;
	FILE	*fp, *fp2;

	if(argc < 2){
		fprintf(stderr, "fmt_un input_file\n");
		exit(1);
	}

	strcpy(tfile, argv[1]);
	if(!(fp = fopen(tfile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", tfile);
		exit(1);
	}

	fscanf(fp, "%[^\n]\n", t1);
	fscanf(fp, "%[^\n]\n", t2);
	fscanf(fp, "%[^\n]\n", t3);
	fclose(fp);

	if(!(fp = fopen(t1, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", t1);
		exit(1);
	}

	fscanf(fp, "%d %d", &nl, &ns);
	fclose(fp);

	if(!(fp = fopen(t2, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", t2);
		exit(1);
	}

	if(!(fp2 = fopen(t3, "w"))){
		fclose(fp);
		fprintf(stderr, "%s: Open failed\n", t3);
		exit(1);
	}
	for(m=0; m<nl; m++){
		for(m2=0; m2<ns; m2++)
			fscanf(fp, "%lf", &i[m2]);
		for(m2=0; m2<ns; m2++)
			fwrite(&i[m2], DBYTES, 1, fp2);
	}
	fclose(fp);
	fclose(fp2);

	exit(0);
}

