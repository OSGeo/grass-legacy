#include <stdio.h>
#include <stdlib.h>

#define	SBYTES		2
#define	DBYTES		8
#define	BUFFER_SIZE	256

#define	TRUE		1
#define	FALSE		0


short	thedir(double mid, double n1, double n2, double n3, double n4,
		double n5, double n6, double n7, double n8);
short	flink(short *center, char *active, short d1, short d2, short d3,
		short d4, short d5, short d6, short d7, short d8, short *select,
		char *activity, char *goagain);


static	short _select[] = {
       0,   1,   2,   2,   4,   1,   2,   2,   8,   1,   8,   2,   8,
       4,   4,   2,  16,  16,  16,   2,  16,   4,   4,   2,   8,   8,
       8,   8,   8,   8,   8,   4,  32,   1,   2,   2,   4,   4,   2,
       2,  32,   8,   8,   2,   8,   8,   4,   4,  32,  32,  32,  32,
      16,  32,   4,   2,  16,  16,  16,  16,   8,  16,   8,   8,  64,
      64,  64,   1,  64,   1,   2,   2,  64,  64,   8,   2,   8,   8,
       4,   2,  16,  64,  64,   2,  16,  64,   2,   2,  16,   8,   8,
       8,   8,   8,   8,   4,  32,  64,  32,   1,  32,  32,  32,   2,
      32,  32,  32,   2,  32,   8,   4,   4,  32,  32,  32,  32,  32,
      32,  32,  32,  32,  32,  16,  16,  16,  16,   8,   8, 128, 128,
     128,   1,   4,   1,   2,   2, 128, 128,   2,   1,   8,   4,   4,
       2,  16, 128,   2,   1,   4, 128,   2,   1,   8, 128,   8,   1,
       8,   8,   4,   2,  32, 128,   1,   1, 128, 128,   2,   1,  32,
     128,  32,   1,   8, 128,   4,   2,  32,  32,  32,   1,  32, 128,
      32,   1,  16,  16,  16,   1,  16,  16,   8,   4, 128, 128, 128,
     128, 128, 128,   2,   1, 128, 128, 128,   1, 128, 128,   4,   2,
      64, 128, 128,   1, 128, 128, 128,   1,   8, 128,   8,   1,   8,
       8,   8,   2,  64, 128,  64, 128,  64, 128,  64, 128,  32,  64,
      64, 128,  64,  64,  64,   1,  32,  64,  64, 128,  64,  64,  64,
     128,  32,  32,  32,  64,  32,  32,  16, 128
};


int
main(int argc, char **argv)
{
	char	difile[BUFFER_SIZE], elfile[BUFFER_SIZE], infile[BUFFER_SIZE],
		system[BUFFER_SIZE];
	short	i1, i2, i3, nl, ns, code, i, ii, j, itemp,
		firstl, lastl, pass, k, im1, im2;
	char	*active, activity, goagain;
	double	**level;
	short	*dir, **sdir, *select = _select - 1;
	FILE	*lfile, *sfile, *tmpfp;

	i1 = 1;
	i2 = 2;
	i3 = 3;
#if 0
	printf("ENTER NL,NS,CODE 1 to RESTART,ELEV FILE,DIR FILE,SYSTEM"
			"(UNIX,VMS,PRIME)\n");
#endif
	scanf("%s", infile);
	if(!(tmpfp = fopen(infile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", infile);
		exit(1);
	}

	fscanf(tmpfp, "%hd %hd %hd", &nl, &ns, &code);
	fscanf(tmpfp, "%s", elfile);
	fscanf(tmpfp, "%s", difile);
	fscanf(tmpfp, "%s", system);
	fclose(tmpfp);

	if(!(lfile = fopen(elfile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", elfile);
		exit(1);
	}

	if(code == 1){
		if(!(sfile = fopen(difile, "r+"))){
			fprintf(stderr, "%s: No such file or open failed\n", difile);
			exit(1);
		}
	}else{
		if(!(sfile = fopen(difile, "w+"))){
			fprintf(stderr, "%s: Open failed\n", difile);
			exit(1);
		}
	}

	dir = (short *)malloc((ns+2)*sizeof(short)) - 1;

	if(code != 1){
		level = (double **)malloc((ns+2)*sizeof(double *)) - 1;
		for(i=1; i<=ns+2; i++)
			level[i] = (double *)malloc(3*sizeof(double)) - 1;

		for(i=1; i<=ns+2; i++){
			dir[i] = 0;
			level[i][1] = 0.0;
		}
		level[1][2] = 0.0;
		level[ns+2][2] = 0.0;
		level[1][3] = 0.0;
		level[ns+2][3] = 0.0;
		for(ii=2; ii<=ns+1; ii++)
			fread(&level[ii][i2], DBYTES, 1, lfile);
		for(i=2; i<=nl+1; i++){
			if(i <= nl){
				for(ii=2; ii<=ns+1; ii++)
					fread(&level[ii][i3], DBYTES, 1, lfile);
			}else{
				for(ii=1; ii<=ns; ii++)
					level[ii][i3] = 0.0;
			}
			for(j=2; j<=ns+1; j++){
				dir[j] = 0;
				if(level[j][i2] == 0.0)
					continue;
				dir[j] = thedir(level[j][i2], level[j+1][i1],
						level[j+1][i2], level[j+1][i3],
						level[j][i3], level[j-1][i3],
						level[j-1][i2], level[j-1][i1],
						level[j][i1]);
			}
			for(ii=2; ii<=ns+1; ii++)
				fwrite(&dir[ii], SBYTES, 1, sfile);
			itemp = i1;
			i1 = i2;
			i2 = i3;
			i3 = itemp;
		}
		fseek(sfile, 0L, SEEK_SET);
		for(i=1; i<=nl; i++){
			for(j=1; j<=ns; j++)
				fread(&dir[j], SBYTES, 1, sfile);
			for(j=1; j<=ns; j++){
				if(dir[j] < 0)
					continue;
				dir[j] = select[dir[j]+1];
			}
			fseek(sfile, (i-1)*ns*SBYTES, SEEK_SET);
			for(j=1; j<=ns; j++)
				fwrite(&dir[j], SBYTES, 1, sfile);
		}

		for(i=1; i<=ns+2; i++)
			free(level[i] + 1);
		free(level + 1);
	}

	active = (char *)malloc(nl*sizeof(char)) - 1;

	for(i=2; i<=nl-1; i++)
		active[i] = TRUE;
	active[1] = FALSE;
	active[nl] = FALSE;
	i1 = 1;
	i2 = 2;
	i3 = 3;
	firstl = 2;
	lastl = nl - 1;
	pass = 0;

	sdir = (short **)malloc((ns+2)*sizeof(short *)) - 1;
	for(i=1; i<=ns+2; i++)
		sdir[i] = (short *)malloc(3*sizeof(short)) - 1;

	for(;;){
		activity = FALSE;
		fseek(sfile, (firstl-2)*ns*SBYTES, SEEK_SET);
		for(j=1; j<=ns; j++)
			fread(&sdir[j][i1], SBYTES, 1, sfile);
		for(j=1; j<=ns; j++)
			fread(&sdir[j][i2], SBYTES, 1, sfile);
		for(j=1; j<=ns; j++)
			fread(&sdir[j][i3], SBYTES, 1, sfile);
		pass++;
		printf("DOWNWARD PASS %2d FIRSTL %2d LASTL %2d\n",
				pass, firstl, lastl);
		i = firstl;
		do{
			active[i] = FALSE;
			do{
				goagain = FALSE;
				for(j=2; j<=ns-1; j++){
					dir[j] = sdir[j][i2];
					if(sdir[j][i2] < 0)
						dir[j] = flink(&sdir[j][i2],
								&active[i],
								sdir[j+1][i1],
								sdir[j+1][i2],
								sdir[j+1][i3],
								sdir[j][i3],
								sdir[j-1][i3],
								sdir[j-1][i2],
								sdir[j-1][i1],
								sdir[j][i1],
								select,
								&activity,
								&goagain);
				}
			}while(goagain == TRUE);
			fseek(sfile, (i-1)*ns*SBYTES, SEEK_SET);
			fwrite(&sdir[1][i2], SBYTES, 1, sfile);
			for(k=2; k<=ns-1; k++)
				fwrite(&dir[k], SBYTES, 1, sfile);
			fwrite(&sdir[ns][i2], SBYTES, 1, sfile);

			do{
				i++;
				if(i > lastl+1)
					break;
				itemp = i1;
				i1 = i2;
				i2 = i3;
				i3 = itemp;
				if(active[i] == FALSE &&
						active[i+1] == FALSE &&
						active[i+2] == FALSE)
					continue;

				fseek(sfile, i*ns*SBYTES, SEEK_SET);
				for(k=1; k<=ns; k++)
					fread(&sdir[k][i3], SBYTES, 1, sfile);
			}while(active[i] == FALSE);
		}while(i <= lastl+1);

		for(i=firstl; i<=lastl; i++)
			if(active[i] == TRUE)
				break;
		if(i > lastl || activity == FALSE){
			if(i <= lastl && activity == FALSE)
				printf("COULD NOT SOLVE FOR ALL CELLS\n");
			break;
		}

		firstl = i;
		for(i=lastl; i>=firstl; i--)
			if(active[i] == TRUE)
				break;
		lastl = i;

		activity = FALSE;
		fseek(sfile, (lastl-2)*ns*SBYTES, SEEK_SET);
		for(j=1; j<=ns; j++)
			fread(&sdir[j][i1], SBYTES, 1, sfile);
		for(j=1; j<=ns; j++)
			fread(&sdir[j][i2], SBYTES, 1, sfile);
		for(j=1; j<=ns; j++)
			fread(&sdir[j][i3], SBYTES, 1, sfile);
		pass++;

		printf("UPWARD PASS %2d FIRSTL %2d LASTL %2d\n",
				pass, firstl, lastl);
		i = lastl;
		do{
			active[i] = FALSE;
			do{
				goagain = FALSE;
				for(j=2; j<=ns-1; j++){
					dir[j] = sdir[j][i2];
					if(sdir[j][i2] < 0)
						dir[j] = flink(&sdir[j][i2],
								&active[i],
								sdir[j+1][i1],
								sdir[j+1][i2],
								sdir[j+1][i3],
								sdir[j][i3],
								sdir[j-1][i3],
								sdir[j-1][i2],
								sdir[j-1][i1],
								sdir[j][i1],
								select,
								&activity,
								&goagain);
				}
			}while(goagain == TRUE);
			fseek(sfile, (i-1)*ns*SBYTES, SEEK_SET);
			fwrite(&sdir[1][i2], SBYTES, 1, sfile);
			for(k=2; k<=ns-1; k++)
				fwrite(&dir[k], SBYTES, 1, sfile);
			fwrite(&sdir[ns][i2], SBYTES, 1, sfile);

			do{
				i--;
				if(i < firstl-1)
					break;
				itemp = i3;
				i3 = i2;
				i2 = i1;
				i1 = itemp;
				im1 = (i-1 > 1 ? i-1 : 1);
				im2 = (i-2 > 1 ? i-2 : 1);
				printf("%4d %4d\n", im1, im2);
				if(active[i] == FALSE &&
						active[im1] == FALSE &&
						active[im2] == FALSE)
					continue;

				fseek(sfile, (i-2)*ns*SBYTES, SEEK_SET);
				for(k=1; k<=ns; k++)
					fread(&sdir[k][i1], SBYTES, 1, sfile);
			}while(active[i] == FALSE);
		}while(i >= firstl-1);

		for(i=lastl; i>=firstl; i--)
			if(active[i] == TRUE)
				break;
		if(i < firstl || activity == FALSE){
			if(i >= firstl && activity == FALSE)
				printf("COULD NOT SOLVE FOR ALL CELLS\n");
			break;
		}

		lastl = i;
		for(i=firstl; i<=lastl; i++)
			if(active[i] == TRUE)
				break;
		firstl = i;
	}

	free(dir + 1);
	free(active + 1);
	for(i=1; i<=ns+2; i++)
		free(sdir[i] + 1);
	free(sdir + 1);

	exit(i < firstl || i > lastl ? 0 : 1);
}


short
thedir(double mid, double n1, double n2, double n3, double n4, double n5,
		double n6, double n7, double n8)
{
	double	*n, maxdrop;
	short	ret, i;

	ret = 0;
	n = (double *)malloc(8*sizeof(double)) - 1;
	n[1] = (mid - n1) / 1.414;
	n[2] =  mid - n2;
	n[3] = (mid - n3) / 1.414;
	n[4] =  mid - n4;
	n[5] = (mid - n5) / 1.414;
	n[6] =  mid - n6;
	n[7] = (mid - n7) / 1.414;
	n[8] =  mid - n8;

	maxdrop = n[1];
	for(i=2; i<=8; i++)
		if(maxdrop < n[i])
			maxdrop = n[i];

	for(i=1; i<=8; i++)
		if(n[i] == maxdrop)
			ret += 1 << (i-1);
	free(n + 1);

	if(maxdrop == 0.0)
		ret *= -1;
	if(maxdrop < 0.0)
		ret = -300;

	return ret;
}


short
flink(short *center, char *active, short d1, short d2, short d3, short d4,
		short d5, short d6, short d7, short d8, short *select,
		char *activity, char *goagain)
{
	short	ret, cwork, i, outflow, bitmask;
	char	*c;

	ret = *center;
	if(ret == -300)
		return ret;

	cwork = -*center;
	c = (char *)malloc(8*sizeof(char)) - 1;
	for(i=8; i>=1; i--){
		c[i] = FALSE;
		bitmask = 1 << (i-1);
		if(cwork-bitmask < 0)
			continue;
		cwork -= bitmask;
		c[i] = TRUE;
	}

	outflow = 0;

	if(d1 != (1 << 4) && d1 > 0 && c[1] == TRUE)
		outflow += 1 << 0;
	if(d2 != (1 << 5) && d2 > 0 && c[2] == TRUE)
		outflow += 1 << 1;
	if(d3 != (1 << 6) && d3 > 0 && c[3] == TRUE)
		outflow += 1 << 2;
	if(d4 != (1 << 7) && d4 > 0 && c[4] == TRUE)
		outflow += 1 << 3;
	if(d5 != (1 << 0) && d5 > 0 && c[5] == TRUE)
		outflow += 1 << 4;
	if(d6 != (1 << 1) && d6 > 0 && c[6] == TRUE)
		outflow += 1 << 5;
	if(d7 != (1 << 2) && d7 > 0 && c[7] == TRUE)
		outflow += 1 << 6;
	if(d8 != (1 << 3) && d8 > 0 && c[8] == TRUE)
		outflow += 1 << 7;

	free(c + 1);

	if(outflow == 0){
		*active = TRUE;

		return ret;
	}
	*center = select[outflow + 1];
	ret = *center;
	*activity = TRUE;
	*goagain = TRUE;

	return ret;
}

