#include <stdio.h>
#include <stdlib.h>


#define	SBYTES		2
#define	DBYTES		8
#define	BUFFER_SIZE	256

#define	TRUE		1
#define	FALSE		0


int
main(int argc, char **argv)
{
	char	difile[BUFFER_SIZE], mafile[BUFFER_SIZE], tfile[BUFFER_SIZE],
		system[BUFFER_SIZE],
		done, activity;
	short	nl, ns, updown, pass, i, i1, i2, i3, ii, index2,
		index, index1, j, dirx, itemp,
		**dir, **mask;
	FILE	*dfile, *mfile, *tmpfp;

#if 0
	printf("ENTER NL,NS,DIR FILE,MASK FILE,SYSTEM(UNIX,VMS,PRIME)\n");
#endif
	scanf("%s", tfile);
	if(!(tmpfp = fopen(tfile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", tfile);
		exit(1);
	}

	fscanf(tmpfp, "%hd %hd", &nl, &ns);
	fscanf(tmpfp, "%s", difile);
	fscanf(tmpfp, "%s", mafile);
	fscanf(tmpfp, "%s", system);
	fclose(tmpfp);

	if(!(dfile = fopen(difile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", difile);
		exit(1);
	}
	if(!(mfile = fopen(mafile, "r+"))){
		fprintf(stderr, "%s: No such file or open failed\n", mafile);
		exit(1);
	}

	mask = (short **)malloc((ns+2)*sizeof(short *)) - 1;
	for(i=1; i<=ns+2; i++)
		mask[i] = (short *)malloc(3*sizeof(short)) - 1;

	dir = (short **)malloc((ns+2)*sizeof(short *)) - 1;
	for(i=1; i<=ns+2; i++)
		dir[i] = (short *)malloc(3*sizeof(short)) - 1;

	updown = -1;
	pass = 0;

	do{
		i1 = 1;
		i2 = 2;
		i3 = 3;
		updown *= -1;
		done = TRUE;
		pass++;
		printf("PASS %2d\n", pass);
		for(ii=1; ii<=ns+2; ii++){
			index = 1;
			if(updown == -1)
				index = 3;
			mask[ii][index] = 0;
		}
		for(i=1; i<=3; i++){
			mask[1][i] = 0;
			mask[ns+2][i] = 0;
		}
		index2 = 0;
		if(updown == -1)
			index2 = nl - 2;
		index1 = 1;
		if(updown == -1)
			index1 = 0;
		fseek(dfile, index2*ns*SBYTES, SEEK_SET);
		fseek(mfile, index2*ns*SBYTES, SEEK_SET);
		for(i=1; i<=2; i++){
			for(ii=2; ii<=ns+1; ii++)
				fread(&dir[ii][i+index1], SBYTES, 1, dfile);
			for(ii=2; ii<=ns+1; ii++)
				fread(&mask[ii][i+index1], SBYTES, 1, mfile);
		}
		for(i=1; i<=nl; i++){
			do{
				activity = FALSE;
				for(j=2; j<=ns+1; j++){
					if(mask[j][i2] == -1){
						dirx = dir[j][i2];
						if(dirx == 0){
							mask[j][i2] = 0;
							activity = TRUE;
						}else
						if(!(mask[j+1][i1] < 0 ||
							dirx != (1 << 0))){
							mask[j][i2] =
								mask[j+1][i1];
							activity = TRUE;
						}else
						if(!(mask[j+1][i2] < 0 ||
							dirx != (1 << 1))){
							mask[j][i2] =
								mask[j+1][i2];
							activity = TRUE;
						}else
						if(!(mask[j+1][i3] < 0 ||
							dirx != (1 << 2))){
							mask[j][i2] =
								mask[j+1][i3];
							activity = TRUE;
						}else
						if(!(mask[j][i3] < 0 ||
							dirx != (1 << 3))){
							mask[j][i2] =
								mask[j][i3];
							activity = TRUE;
						}else
						if(!(mask[j-1][i3] < 0 ||
							dirx != (1 << 4))){
							mask[j][i2] =
								mask[j-1][i3];
							activity = TRUE;
						}else
						if(!(mask[j-1][i2] < 0 ||
							dirx != (1 << 5))){
							mask[j][i2] =
								mask[j-1][i2];
							activity = TRUE;
						}else
						if(!(mask[j-1][i1] < 0 ||
							dirx != (1 << 6))){
							mask[j][i2] =
								mask[j-1][i1];
							activity = TRUE;
						}else
						if(!(mask[j][i1] < 0 ||
							dirx != (1 << 7))){
							mask[j][i2] =
								mask[j][i1];
							activity = TRUE;
						}
					}
					if(activity == TRUE)
						done = FALSE;
				}
			}while(activity == TRUE);
			index = i;
			if(updown == -1)
				index = nl - i + 1;
			fseek(mfile, (index-1)*ns*SBYTES, SEEK_SET);
			for(ii=2; ii<=ns+1; ii++)
				fwrite(&mask[ii][i2], SBYTES, 1, mfile);
			if(i == nl)
				continue;
			if(updown != -1){
				itemp = i1;
				i1 = i2;
				i2 = i3;
				i3 = itemp;
				if(i == nl-1){
					for(ii=1; ii<=ns+2; ii++)
						mask[ii][i3] = 0;
				}else{
					fseek(mfile, (i+1)*ns*SBYTES, SEEK_SET);
					for(ii=2; ii<=ns+1; ii++)
						fread(&mask[ii][i3], SBYTES, 1,
								mfile);
					fseek(dfile, (i+1)*ns*SBYTES, SEEK_SET);
					for(ii=2; ii<=ns+1; ii++)
						fread(&dir[ii][i3], SBYTES, 1,
								dfile);
				}
			}else{
				itemp = i3;
				i3 = i2;
				i2 = i1;
				i1 = itemp;
				if(i == nl-1){
					for(ii=1; ii<=ns+2; ii++)
						mask[ii][i1] = 0;
				}else{
					fseek(mfile, (nl-i-2)*ns*SBYTES,
							SEEK_SET);
					for(ii=2; ii<=ns+1; ii++)
						fread(&mask[ii][i1], SBYTES, 1,
								mfile);
					fseek(dfile, (nl-i-2)*ns*SBYTES,
							SEEK_SET);
					for(ii=2; ii<=ns+1; ii++)
						fread(&dir[ii][i1], SBYTES, 1,
								dfile);
				}
			}
		}
	}while(done == FALSE);

	for(i=1; i<=ns+2; i++)
		free(mask[i] + 1);
	free(mask + 1);

	for(i=1; i<=ns+2; i++)
		free(dir[i] + 1);
	free(dir + 1);

	exit(0);
}

