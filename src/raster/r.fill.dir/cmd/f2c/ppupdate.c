#include <stdio.h>
#include <stdlib.h>


#define	SBYTES		2
#define	DBYTES		8
#define	BUFFER_SIZE	256

#define	TRUE		1
#define	FALSE		0

#define	mabs(x)		((x) >= 0 ? (x) : -(x))

#define	_stack(i)	((*mask)[i][1])
#define	_mapto(i, j)	((*mask)[((j)-1)*2+(i)][2])


void	do3by3(short wksamp, short line, short i1, short i2, short i3,
		short **mask, double **elev, short ***allpptbl, double ***ltbl);
void	allpp(short m1, short m2, double e1, double e2, short l1, short l2,
		short ***allpptbl, double ***ltbl);
void	connect(short nl, short ns, FILE *lunit, FILE *munit,
		short ***allpptbl, double ***ltbl, short ***mask);
void	lowpp(short *allpptot, short ***allpptbl, double ***ltbl);
void	path(char *done, short *num, short *loop, short allpptot,
		short ***allpptbl, short ***mask);
void	fixaloop(short *loop, short stacktot, short allpptot, short *maptonum,
		short ***allpptbl, double ***ltbl, short ***mask);
void	update(short allpptot, short num,
		short ***allpptbl, double ***ltbl, short ***mask);
void	domapto(short *maptonum, short *members, short num, short loopend,
		short ***mask);
void	fill(short allpptot, short maptonum, FILE *lunit, FILE *munit,
		short nl, short ns, short ***allpptbl, double ***ltbl,
		short ***mask);

char	err;


int
main(int argc, char **argv)
{
	char	bafile[BUFFER_SIZE], elfile[BUFFER_SIZE], tfile[BUFFER_SIZE],
		system[BUFFER_SIZE];
	double	**elev, **ltbl;
	short	nl, ns, i, i1, i2, i3, maxb, line, samp, index, itemp,
		**mask, **allpptbl;
	FILE	*lunit, *munit, *tmpfp;

#if 0
	printf("ENTER NL,NS,BASIN FILE,ELEV FILE,SYSTEM(UNIX,VMS,PRIME)\n");
#endif
	scanf("%s", tfile);
	if(!(tmpfp = fopen(tfile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", tfile);
		exit(1);
	}

	fscanf(tmpfp, "%hd %hd", &nl, &ns);
	fscanf(tmpfp, "%s", bafile);
	fscanf(tmpfp, "%s", elfile);
	fscanf(tmpfp, "%s", system);
	fclose(tmpfp);

	if(!(munit = fopen(bafile, "r"))){
		fprintf(stderr, "%s: No such file or open failed\n", bafile);
		exit(1);
	}
	if(!(lunit = fopen(elfile, "r+"))){
		fprintf(stderr, "%s: No such file or open failed\n", elfile);
		exit(1);
	}

	mask = (short **)malloc((ns+2)*sizeof(short *)) - 1;
	for(i=1; i<=ns+2; i++)
		mask[i] = (short *)malloc(3*sizeof(short)) - 1;

	elev = (double **)malloc((ns+2)*sizeof(double *)) - 1;
	for(i=1; i<=ns+2; i++)
		elev[i] = (double *)malloc(3*sizeof(double)) - 1;

	allpptbl = (short **)malloc(3*sizeof(short *)) - 1;
	for(i=1; i<=3; i++)
		allpptbl[i] = (short *)malloc(2000*sizeof(short)) - 1;

	ltbl = (double **)malloc(2*sizeof(double *)) - 1;
	for(i=1; i<=2; i++)
		ltbl[i] = (double *)malloc(2000*sizeof(double)) - 1;

	i1 = 1;
	i2 = 2;
	i3 = 3;
	maxb = 0;
	for(i=1; i<=ns+2; i++){
		mask[i][i1] = 0;
		elev[i][i1] = 0.0;
	}
	for(i=1; i<=2000; i++)
		allpptbl[1][i] = -1;
	for(i=2; i<=ns+1; i++)
		fread(&mask[i][i2], SBYTES, 1, munit);
	for(line=1; line<=nl; line++){
		if(line != nl){
			fseek(munit, line*ns*SBYTES, SEEK_SET);
			for(i=2; i<=ns+1; i++)
				fread(&mask[i][i3], SBYTES, 1, munit);
			fseek(lunit, line*ns*DBYTES, SEEK_SET);
			for(i=2; i<=ns+1; i++)
				fread(&elev[i][i3], DBYTES, 1, lunit);
		}else{
			for(i=2; i<=ns+1; i++){
				mask[i][i3] = 0;
				elev[i][i3] = 0.0;
			}
		}
		for(samp=2; samp<=ns+1; samp++){
			index = mask[samp][i2];
			if(index == 0)
				continue;
			err = 0;
			do3by3(samp, line, i1, i2, i3, mask, elev,
					&allpptbl, &ltbl);
			if(err){
				for(i=1; i<=ns+2; i++)
					free(mask[i] + 1);
				free(mask + 1);

				for(i=1; i<=ns+2; i++)
					free(elev[i] + 1);
				free(elev + 1);

				for(i=1; i<=3; i++)
					free(allpptbl[i] + 1);
				free(allpptbl + 1);

				for(i=1; i<=2; i++)
					free(ltbl[i] + 1);
				free(ltbl + 1);

				exit(1);
			}
		}
		itemp = i1;
		i1 = i2;
		i2 = i3;
		i3 = itemp;
	}

	connect(nl, ns, lunit, munit, &allpptbl, &ltbl, &mask);

	for(i=1; i<=ns+2; i++)
		free(mask[i] + 1);
	free(mask + 1);

	for(i=1; i<=ns+2; i++)
		free(elev[i] + 1);
	free(elev + 1);

	for(i=1; i<=3; i++)
		free(allpptbl[i] + 1);
	free(allpptbl + 1);

	for(i=1; i<=2; i++)
		free(ltbl[i] + 1);
	free(ltbl + 1);

	exit(0);
}


void
do3by3(short wksamp, short line, short i1, short i2, short i3,
		short **mask, double **elev, short ***allpptbl, double ***ltbl)
{
	static	short _linc[] = {-1, -1, -1, 0, 0, 0, 1, 1, 1};
	double	*e;
	short	i, *m, *linc = _linc - 1;
	char	elog;

	e = (double *)malloc(9*sizeof(double)) - 1;
	m = (short *)malloc(9*sizeof(short)) - 1;

	for(i=1; i<=3; i++){
		m[i] = mask[wksamp+i-2][i1];
		e[i] = elev[wksamp+i-2][i1];
		m[i+3] = mask[wksamp+i-2][i2];
		e[i+3] = elev[wksamp+i-2][i2];
		m[i+6] = mask[wksamp+i-2][i3];
		e[i+6] = elev[wksamp+i-2][i3];
	}

	elog = FALSE;
	for(i=1; i<=9; i++)
		if(mabs(m[i]) != mabs(m[5]))
			elog = TRUE;
	if(elog == TRUE){
		for(i=1; i<=9; i++){
			allpp(m[i], m[5], e[i], e[5], line+linc[i], line,
					allpptbl, ltbl);
			if(err)
				break;
		}
	}

	free(e + 1);
	free(m + 1);

	return;
}


void
allpp(short m1, short m2, double e1, double e2, short l1, short l2,
		short ***allpptbl, double ***ltbl)
{
	short	bigm, littlem, i;
	double	maxe, maxltbl;

	if(m1 == m2)
		return;

	bigm = (m1 > m2 ? m1 : m2);
	littlem = (m1 < m2 ? m1 : m2);
	for(i=1; i<=2000; i++){
		if((*allpptbl)[1][i] != -1){
			if((*allpptbl)[1][i] != littlem ||
			   (*allpptbl)[2][i] != bigm){
				if(i == 2000){
					err = 1;
					return;
				}
				continue;
			}
			maxe = (e1 > e2 ? e1 : e2);
			maxltbl = ((*ltbl)[1][i] > (*ltbl)[2][i] ?
					(*ltbl)[1][i] : (*ltbl)[2][i]);
			if(maxe >= maxltbl)
				return;
		}else{
			(*allpptbl)[1][i] = littlem;
			(*allpptbl)[2][i] = bigm;
		}
		(*ltbl)[1][i] = e1;
		(*ltbl)[2][i] = e2;
		(*allpptbl)[3][i] = l1;
		if(m1 == littlem)
			return;
		(*ltbl)[1][i] = e2;
		(*ltbl)[2][i] = e1;
		(*allpptbl)[3][i] = l2;
		break;
	}

	return;
}


void
connect(short nl, short ns, FILE *lunit, FILE *munit,
		short ***allpptbl, double ***ltbl, short ***mask)
{
	short	maptonum, allpptot, num, loop;
	char	done;

	maptonum = 0;
	lowpp(&allpptot, allpptbl, ltbl);
	done = FALSE;

	for(;;){
		path(&done, &num, &loop, allpptot, allpptbl, mask);
		if(done == TRUE)
			break;
		if(loop != 0)
			fixaloop(&loop, num, allpptot, &maptonum,
					allpptbl, ltbl, mask);
		else
			update(allpptot, num, allpptbl, ltbl, mask);
	}

	fill(allpptot, maptonum, lunit, munit, nl, ns, allpptbl, ltbl, mask);

	return;
}


void
lowpp(short *allpptot, short ***allpptbl, double ***ltbl)
{
	char	cont;
	short	i, j, k, try, sindex;
	double	smallest;

	for(i=1; i<=2000; i++){
		if((*allpptbl)[1][i] == -1)
			break;
		(*ltbl)[1][i] = ((*ltbl)[1][i] > (*ltbl)[2][i] ?
				 (*ltbl)[1][i] : (*ltbl)[2][i]);
		(*allpptbl)[3][i] = 0;
	}

	*allpptot = i - 1;
	for(i=1; i<=*allpptot; i++){
		for(j=1; j<=2; j++){
			try = (*allpptbl)[j][i];
			if(try == 0)
				continue;
			smallest = 0.0;
			cont = 0;
			for(k=1; k<=*allpptot; k++){
				if((*allpptbl)[1][k] == try &&
				   (*allpptbl)[3][k] == 1){
					cont = 1;
					break;
				}
				if(((*allpptbl)[1][k] != try &&
				    (*allpptbl)[2][k] != try) ||
				   (smallest != 0.0 &&
				    (*ltbl)[1][k] >= smallest))
					continue;
				smallest = (*ltbl)[1][k];
				sindex = k;
			}
			if(cont)
				continue;
			if((*allpptbl)[3][sindex] != 1){
				(*allpptbl)[3][sindex] = 1;
				if((*allpptbl)[1][sindex] == try)
					continue;
				(*allpptbl)[2][sindex] = (*allpptbl)[1][sindex];
				(*allpptbl)[1][sindex] = try;
				continue;
			}
			(*allpptot)++;
			(*allpptbl)[1][*allpptot] = try;
			(*allpptbl)[2][*allpptot] = (*allpptbl)[1][sindex];
			(*ltbl)[1][*allpptot] = smallest;
			(*allpptbl)[3][*allpptot] = 1;
		}
	}

	return;
}


void
path(char *done, short *num, short *loop, short allpptot,
		short ***allpptbl, short ***mask)
{
	char	cont;
	short	i, j, from;

	*loop = 0;
	for(i=1; i<=allpptot; i++){
		if(!((*allpptbl)[1][i] == -2 ||
		     (*allpptbl)[3][i] == 0 ||
		     (*allpptbl)[2][i] == 0))
			break;
	}
	if(i > allpptot){
		*done = 1;
		return;
	}

	*num = 1;
	_stack(1) = i;

	do{
		cont = 0;
		from = (*allpptbl)[2][i];
		for(i=1; i<=allpptot; i++){
			if(!((*allpptbl)[3][i] != 1 ||
					(*allpptbl)[1][i] != from)){
				(*num)++;
				_stack(*num) = i;
				if((*allpptbl)[2][i] == 0)
					return;
				for(j=1; j<=*num-1; j++){
					if(_stack(j) == _stack(*num)){
						*loop = j;
						return;
					}
				}
				cont = 1;
				break;
			}
		}
	}while(cont);

	return;
}


void
fixaloop(short *loop, short stacktot, short allpptot, short *maptonum,
		short ***allpptbl, double ***ltbl, short ***mask)
{
	short	num, loopend, i, j, ppindex, start,
		*members;
	double	smallest;

	members = (short *)malloc(2000*sizeof(short)) - 1;

	start = *loop + 1;
	*loop = stacktot;
	num = *loop - start + 1;
	loopend = (*allpptbl)[1][_stack(*loop)];

	for(i=1; i<=num; i++)
		members[i] = (*allpptbl)[1][_stack(start-1+i)];

	domapto(maptonum, members, num, loopend, mask);

	for(i=1; i<=allpptot; i++){
		if((*allpptbl)[1][i] == -2)
			continue;
		for(j=1; j<=num; j++)
			if((*allpptbl)[1][i] == members[j])
				break;
		if(j > num)
			continue;
		for(j=1; j<=num; j++)
			if((*allpptbl)[2][i] == members[j])
				break;
		if(j > num)
			continue;
		(*allpptbl)[1][i] = -2;
	}

	for(i=1; i<=allpptot; i++){
		if((*allpptbl)[1][i] == -2)
			continue;
		for(j=1; j<=num-1; j++){
			if((*allpptbl)[1][i] == members[j])
				(*allpptbl)[1][i] = loopend;
			if((*allpptbl)[2][i] == members[j])
				(*allpptbl)[2][i] = loopend;
		}
	}

	smallest = 0.0;
	for(i=1; i<=allpptot; i++){
		if((*allpptbl)[1][i] == -2 ||
		   ((*allpptbl)[1][i] != loopend &&
		    (*allpptbl)[2][i] != loopend) ||
		   (smallest != 0.0 && (*ltbl)[1][i] > smallest))
			continue;
		smallest = (*ltbl)[1][i];
		ppindex = i;
	}

	if((*allpptbl)[3][ppindex] != 0){
		allpptot++;
		(*allpptbl)[1][allpptot] = loopend;
		(*allpptbl)[2][allpptot] = (*allpptbl)[1][ppindex];
		(*ltbl)[1][allpptot] = smallest;
		(*allpptbl)[3][allpptot] = 1;
		(*allpptbl)[1][allpptot+1] = -1;
	}else{
		(*allpptbl)[3][ppindex] = 1;
		if((*allpptbl)[1][ppindex] != loopend){
			(*allpptbl)[2][ppindex] = (*allpptbl)[1][ppindex];
			(*allpptbl)[1][ppindex] = loopend;
		}
	}

	free(members + 1);

	return;
}


void
update(short allpptot, short num,
		short ***allpptbl, double ***ltbl, short ***mask)
{
	short	i, j;
	double	biggest;

	for(i=1; i<=num-1; i++){
		biggest = (*ltbl)[1][_stack(i)];
		for(j=i+1; j<=num; j++)
			biggest = ((*ltbl)[1][_stack(j)] > biggest ?
					(*ltbl)[1][_stack(j)] : biggest);
		(*ltbl)[1][_stack(i)] = biggest;
		(*allpptbl)[2][_stack(i)] = 0;
	}

	return;
}


void
domapto(short *maptonum, short *members, short num, short loopend,
		short ***mask)
{
	short	tolink, i, j, try;

	tolink = loopend;
	if(*maptonum != 0){
		for(i=1; i<=*maptonum; i++){
			if(_mapto(1, i) == loopend)
				tolink = _mapto(2, i);
		}
		for(i=1; i<=num; i++){
			try = members[i];
			for(j=1; j<=*maptonum; j++){
				if(_mapto(2, j) == try)
					_mapto(2, j) = tolink;
			}
		}
	}

	for(i=1; i<=num-1; i++){
		(*maptonum)++;
		_mapto(1, *maptonum) = members[i];
		_mapto(2, *maptonum) = tolink;
	}

	return;
}


void
fill(short allpptot, short maptonum, FILE *lunit, FILE *munit,
		short nl, short ns, short ***allpptbl, double ***ltbl,
		short ***mask)
{
	short	i, j, *ibuffer;
	double	ppelev, *dbuffer;

	for(i=1; i<=allpptot; i++){
		if(!((*allpptbl[1][i] == -2 || (*allpptbl)[3][i] != 1)))
			(*ltbl)[2][(*allpptbl)[1][i]] = (*ltbl)[1][i];
	}
	for(i=1; i<=maptonum; i++)
		(*ltbl)[2][_mapto(1, i)] = (*ltbl)[2][_mapto(2, i)];

	ibuffer = (short *)malloc(ns*sizeof(short)) - 1;
	dbuffer = (double *)malloc(ns*sizeof(double)) - 1;

	for(i=1; i<=nl; i++){
		fseek(munit, (i-1)*ns*SBYTES, SEEK_SET);
		for(j=1; j<=ns; j++)
			fread(&ibuffer[j], SBYTES, 1, munit);
		fseek(lunit, (i-1)*ns*DBYTES, SEEK_SET);
		for(j=1; j<=ns; j++)
			fread(&dbuffer[j], DBYTES, 1, lunit);
		for(j=1; j<=ns; j++){
			if(ibuffer[j] > 0){
				ppelev = (*ltbl)[2][ibuffer[j]];
				if(dbuffer[j] < ppelev)
					dbuffer[j] = ppelev;
			}
		}
		fseek(lunit, (i-1)*ns*DBYTES, SEEK_SET);
		for(j=1; j<=ns; j++)
			fwrite(&dbuffer[j], DBYTES, 1, lunit);
	}

	free(ibuffer + 1);
	free(dbuffer + 1);

	return;
}

