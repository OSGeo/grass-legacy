#include <stdio.h>
#include <stdlib.h>


//#define	SBYTES		2
//#define	DBYTES		8
#define	BUFFER_SIZE	256


int
main(int argc, char **argv)
{
	short	i;
	float	r;
	double	d;
	short	dbytes, nl, ns, m, ii, input_type, output_type;
	char	tfile[BUFFER_SIZE],
		t1[BUFFER_SIZE], t2[BUFFER_SIZE], t3[BUFFER_SIZE];
	FILE	*fp, *fp2;

	scanf("%s", tfile);
	if(!(fp = fopen(tfile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", tfile);
		exit(1);
	}

	fscanf(fp, "%hd %hd", &input_type, &output_type);
	fscanf(fp, "%s", t1);
	fscanf(fp, "%s", t2);
	fscanf(fp, "%s", t3);
	fclose(fp);

	if(!(fp = fopen(t1, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", t1);
		exit(1);
	}

	fscanf(fp, "%hd %hd", &nl, &ns);
	fclose(fp);

	if(!(fp = fopen(t2, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", t2);
		exit(1);
	}

	if(!(fp2 = fopen(t3, "w"))){
		fprintf(stderr, "%s: Open failed\n", t3);
		exit(1);
	}

	if(input_type < 0 || input_type > 2 ||
			output_type < 0 || output_type > 2)
		exit(1);

	dbytes = 1 << (input_type + 1);

	for(m=1; m<=nl; m++){
		for(ii=1; ii<=ns; ii++){
			switch(input_type){
			case 0:
				fread(&i, dbytes, 1, fp);
				switch(output_type){
				case 0:
					fwrite(&i, dbytes, 1, fp2);
					break;
				case 1:
					r = (float) i;
					fwrite(&r, dbytes, 1, fp2);
					break;
				case 2:
					d = (double) i;
					fwrite(&d, dbytes, 1, fp2);
					break;
				}
				break;
			case 1:
				fread(&r, dbytes, 1, fp);
				switch(output_type){
				case 0:
					i = (short) r;
					fwrite(&i, dbytes, 1, fp2);
					break;
				case 1:
					fwrite(&r, dbytes, 1, fp2);
					break;
				case 2:
					d = (double) r;
					fwrite(&d, dbytes, 1, fp2);
					break;
				}
				break;
			case 2:
				fread(&d, dbytes, 1, fp);
				switch(output_type){
				case 0:
					i = (short) r;
					fwrite(&i, dbytes, 1, fp2);
					break;
				case 1:
					r = (float) d;
					fwrite(&r, dbytes, 1, fp2);
					break;
				case 2:
					d = (double) r;
					fwrite(&d, dbytes, 1, fp2);
					break;
				}
				break;
			}
		}
	}

	exit(0);
}

