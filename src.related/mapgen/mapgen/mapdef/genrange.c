#ifndef lint
static char *SCCSID = "@(#)genrange.c	AMG v.3.2";
#endif
/* proc. for 'mapdef' */
# include <stdio.h>
# include <string.h>
# include <math.h>
# include <ctype.h>
# include "mapgen.h"
# include "mapdef.h"

# define MAXARG 20
# define C_MAX	25

/* approximation degree */
# define APRX_DEG	8
# define MAX_A_DEG	12

static loadapr();

/* generate lat-lon grid for approximations */
genrange() {
	int i, j, maxdeg;
	long strtol();
	FILE *file;
	struct {
		double x, y;
	} p;
	double *v, x_del, y_del, dmstor();
	char *sv, *s, cmarg[C_MAX], *tempnam(), cline[180];

start:
	if (prompt) printf(
		"enter left, right longitude, bottom, top \
latitude [central meridian] (all DMS)\n");

	if (getline(line, MAXLINE, stdin) == 0)
		quit("range line", 1);
	v = &(def.l_lon);
	for (s = line, i = 0; i < 5; i++) {
		if ((sv = strtok(s, " \t")) != NULL) {
			if ((*v++ = dmstor(sv, 0)) == HUGE) {
				quit("bad range value\n", 0);
				goto start;
			}
		} else if (i < 4) {
			quit("insufficient range arguments\n", 0);
			goto start;
		} else
			break;
		s = NULL;
	}

	while (def.l_lon >= def.r_lon)
		def.l_lon -= TWO_PI;
	if (def.b_lat == def.t_lat) {
		quit("latitudes equal",0);
		goto start;
	}
	if (i < 5) { /* no - central meridian */
		def.cm = .5 * (def.l_lon + def.r_lon);
		if (verbose) {
			printf("central merdian computed is ");
			rtodms(line, def.cm, 'e', 'w');
			printf("%s\n", line);
		}
	}

start2:
	if (prompt)
		printf("enter projection parameters (+):\n");
	if (getline(def.proj, MAXPROJ - C_MAX, stdin) == 0)
		quit("proj input null", 1);
	sprintf(cmarg," +lon_0=%.12f",R_TO_DEG * def.cm);
	strcat(def.proj, cmarg); /* save in 'def' file */
	s = def.proj;
	while (isspace(*s)) ++s;
	s = strcpy(line, s);
	maxdeg = APRX_DEG;
	if (isdigit(*s)) {
		maxdeg = strtol(s, &s, 10);
		if (maxdeg <= 1)
			maxdeg = APRX_DEG;
		else if (maxdeg > MAX_A_DEG) {
			quit("bad poly degree too high", 0);
			goto start2;
		}
	}
	while (isspace(*s)) ++s;
		/* generate coordinate grid */
	if (verbose)
		printf("performing projection of calibration grid\n");
	if (!(f_tempb = tempnam(TEMP, "b")) ||
		!(f_tempa = tempnam(TEMP, "a")) ||
		!(file = fopen(f_tempa, "w")))
		quit("can't handle temporary file opening",1);
	x_del = .1 * (def.r_lon - def.l_lon);
	y_del = .1 * (def.t_lat - def.b_lat);
	p.y = def.b_lat;
	for (j = 0; j <= 10 ; j++) {
		p.x = def.l_lon;
		for (i = 0; i <= 10; i++) {
			fprintf(file,"%.13f\t%.13f\n", R_TO_DEG * p.x,
				R_TO_DEG * p.y);
			p.x += x_del;
		}
		p.y += y_del;
	}
	fclose(file);
	sprintf(cline, "%s %s <%s >$$temp ; paste %s $$temp >%s ;rm -f $$temp",
		PROJ_PROG, s, f_tempa, f_tempa, f_tempb);
		/* call x-approximation */
	if (system(cline) < 0) {
		perror("cline");
		quit("system call failure",1);
	}
	if (verbose)
		printf("determining x-axis approximation\n");
	sprintf(cline,"%s -cmf %d '%%le %%le %%le %%*e' %s <%s >%s",
		APPROX, maxdeg, verbose?"-v":"",f_tempb,f_tempa);
	if (system(cline) < 0) {
		perror("cline");
		quit("system call failure",1);
	}
	loadapr(&def.nxC, def.xC, f_tempa);
		/* call y-approximation */
	if (verbose)
		printf("determining y-axis approximation\n");
	sprintf(cline,"%s -cmf %d '%%le %%le %%*e %%le' %s <%s >%s",
		APPROX, maxdeg, verbose?"-v":"",f_tempb,f_tempa);
	if (system(cline) < 0) {
		perror("cline");
		quit("system call failure",1);
	}
	loadapr(&def.nyC, def.yC, f_tempa);
	unlink(f_tempb);  f_tempb = 0;
	unlink(f_tempa);  f_tempa = 0;
		/* set remainder of appoximation control */
	if (verbose)
		printf("projection approximations completed\n");
	def.x_scale *= R_TO_DEG;
	def.y_scale *= R_TO_DEG;
		/* done with this phase */

	return;
}
	static
loadapr(nC, C, file) int *nC; double *C; char *file; {
	int i;
	FILE *fid;

	if ((fid = fopen(file,"r")) == NULL) {
		perror(file);
		quit("can't open coef. input", 1);
	}
	fscanf(fid,"%d %d %d %d",&i,&def.x_deg,&def.y_deg,nC);
	if (*nC >= MAX_C)
		*nC = MAX_C;
	fscanf(fid,"%lf %lf %*f %*f %lf %lf %*f %*f",
		&def.x_scale, &def.x_off, &def.y_scale, &def.y_off);
	for ( i = 0; i < *nC; i++)
		fscanf(fid,"%lf",C++);
	fclose(fid);
}
