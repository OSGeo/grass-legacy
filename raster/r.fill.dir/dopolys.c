#include <stdio.h>
#include <stdlib.h>


#define	SBYTES		2
#define	DBYTES		8
#define	BUFFER_SIZE	256


short	bridge(short m1, short m2, short m3, short m4, short **mapto);

char	err;


int
main(int argc, char **argv)
{
	char	lifile[BUFFER_SIZE], mafile[BUFFER_SIZE], tfile[BUFFER_SIZE],
		system[BUFFER_SIZE];
	short	nl, ns, i, i1, i2, ii, j, curlabel, m1, m2, m3, m4,
		k, kk, itemp, totlab, num, test, check, m,
		*sdir,
		**mask,
		**mapto,
		*link, *stack;
	FILE	*lfile, *mfile, *tmpfp;

#if 0
	printf("ENTER NL,NS,LINK FILE,MASK FILE,SYSTEM(UNIX,VMS,PRIME)\n");
#endif
	scanf("%s", tfile);
	if(!(tmpfp = fopen(tfile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", tfile);
		exit(1);
	}

	fscanf(tmpfp, "%hd %hd", &nl, &ns);
	fscanf(tmpfp, "%s", lifile);
	fscanf(tmpfp, "%s", mafile);
	fscanf(tmpfp, "%s", system);
	fclose(tmpfp);

	if(!(lfile = fopen(lifile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", tfile);
		exit(1);
	}

	if(!(mfile = fopen(mafile, "w+"))){
		fprintf(stderr, "%s: Open failed\n", mafile);
		exit(1);
	}

	i1 = 1;
	i2 = 2;
	curlabel = 1;

	sdir = (short *)malloc(ns*sizeof(short)) - 1;
	mask = (short **)malloc(ns*sizeof(short *)) - 1;
	for(i=1; i<=ns; i++){
		mask[i] = (short *)malloc(2*sizeof(short)) - 1;
		mask[i][1] = -1;
	}

	mapto = (short **)malloc(21*sizeof(short *)) - 1;
	for(i=1; i<=21; i++)
		mapto[i] = (short *)malloc(5000*sizeof(short)) - 1;

	for(ii=1; ii<=ns; ii++)
		fwrite(&mask[ii][1], SBYTES, 1, mfile);

	fseek(lfile, ns*SBYTES, SEEK_SET);
	for(ii=1; ii<=ns; ii++)
		fread(&sdir[ii], SBYTES, 1, lfile);

	for(i=2; i<=nl-1; i++){
		mask[1][i2] = -1;
		mask[ns][i2] = -1;
		for(j=2; j<=ns-1; j++){
			mask[j][i2] = -1;
			if(sdir[j] == 0.0)
				mask[j][i2] = 0;
			if(sdir[j] >= 0.0)
				continue;
			m1 = mask[j-1][i1];
			m2 = mask[j][i1];
			m3 = mask[j+1][i1];
			m4 = mask[j-1][i2];

			err = 0;
			if(m1 > 0 || m2 > 0 || m3 > 0 || m4 > 0)
				mask[j][i2] = bridge(m1, m2, m3, m4, mapto);
			else{
				mask[j][i2] = curlabel;
				for(kk=1; kk<=21; kk++)
					mapto[kk][curlabel] = 0;
				curlabel++;
				if(curlabel > 5000)
					err = 1;
			}
			if(err){
				free(sdir + 1);
				for(i=1; i<=ns; i++)
					free(mask[i] + 1);
				free(mask + 1);

				for(i=1; i<=21; i++)
					free(mapto[i] + 1);
				free(mapto + 1);

				exit(1);
			}
			
		}

		fseek(mfile, (i-1)*ns*SBYTES, SEEK_SET);
		for(ii=1; ii<=ns; ii++)
			fwrite(&mask[ii][i2], SBYTES, 1, mfile);
		itemp = i1;
		i1 = i2;
		i2 = itemp;
		fseek(lfile, i*ns*SBYTES, SEEK_SET);
		for(ii=1; ii<=ns; ii++)
			fread(&sdir[ii], SBYTES, 1, lfile);
	}

	fseek(mfile, (nl-1)*ns*SBYTES, SEEK_SET);
	for(i=1; i<=ns; i++){
		mask[i][1] = -1;
		fwrite(&mask[i][1], SBYTES, 1, mfile);
	}

	totlab = curlabel - 1;
	printf("FOUND %2d POLYGON PARTS\n", totlab);

	link = (short *)malloc(totlab*sizeof(short)) - 1;
	stack = (short *)malloc(5000*sizeof(short)) - 1;
	for(i=1; i<=totlab; i++)
		link[i] = i;
	for(i=1; i<=totlab; i++){
		if(link[i] != i || mapto[1][i] == 0)
			continue;
		num = mapto[1][i];
		for(j=1; j<=num; j++)
			stack[j] = mapto[j+1][i];
		ii = 1;
		do{
			test = stack[ii];
			if(mapto[1][test] != 0){
				for(j=2; j<=mapto[1][test]+1; j++){
					check = mapto[j][test];
					for(k=1; k<=num; k++)
						if(check == stack[k])
							break;
					if(k<=num)
						continue;
					num++;
					stack[num] = check;
				}
			}
			ii++;
		}while(ii <= num);

		for(j=1; j<=num; j++)
			link[stack[j]] = i;
	}

	for(i=2; i<=nl-1; i++){
		fseek(mfile, (i-1)*ns*SBYTES, SEEK_SET);
		for(ii=1; ii<=ns; ii++)
			fread(&mask[ii][1], SBYTES, 1, mfile);
		for(j=2; j<=ns-1; j++){
			m = mask[j][1];
			if(m == -1 || m == 0)
				continue;
			mask[j][1] = link[m];
		}
		fseek(mfile, (i-1)*ns*SBYTES, SEEK_SET);
		for(ii=1; ii<=ns; ii++)
			fwrite(&mask[ii][1], SBYTES, 1, mfile);
	}


	free(sdir + 1);
	for(i=1; i<=ns; i++)
		free(mask[i] + 1);
	free(mask + 1);

	for(i=1; i<=21; i++)
		free(mapto[i] + 1);
	free(mapto + 1);
	free(link + 1);
	free(stack + 1);

	exit(0);
}


short
bridge(short m1, short m2, short m3, short m4, short **mapto)
{
	short	ret, *m, i, mi, j, mj, num, k;

	m = (short *)malloc(4*sizeof(short)) - 1;
	m[1] = m1;
	m[2] = m2;
	m[3] = m3;
	m[4] = m4;
	i = 4;
	if(m4 <= 0){
		for(i=1; i<=3; i++)
			if(m[i] > 0)
				break;
		if(i > 3){
			free(m + 1);
			err = 1;
			return 0;
		}
	}

	ret = m[i];
	for(i=1; i<=4; i++){
		if(m[i] <= 0)
			continue;
		mi = m[i];
		for(j=1; j<=4; j++){
			if(mi == m[j] || m[j] <= 0)
				continue;
			mj = m[j];
			num = mapto[1][mi];
			if(num != 0){
				for(k=1; k<=num; k++)
					if(mapto[k+1][mi] == mj)
						break;
				if(k <= num)
					continue;
			}
			mapto[1][mi]++;
			if(mapto[1][mi] > 20){
				free(m + 1);
				err = 1;
				return 0;
			}
			mapto[mapto[1][mi]+1][mi] = mj;
			mapto[1][mj]++;
			if(mapto[1][mj] > 20){
				free(m + 1);
				err = 1;
				return 0;
			}
			mapto[mapto[1][mj]+1][mj] = mi;
		}
	}

	free(m + 1);

	return ret;
}

