#ifndef lint
static char *SCCSID = "@(#)szoom.c	OEMG v.1.4";
#endif
/* szoom: sunview/sunCore version of zoom */
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <plotgen.h>
#include <mapgen.h>
FILE	*logfid;
	struct map_def
m_def;
	struct PLTDEF
def;
	double
cts;
# define GRAPHB "GRAPHB"
# define pXMAX 1000.
# define pYMAX 750.
# define pXOFF 90.
# define pYOFF 50.
# define pXSIZ pXMAX-pXOFF
# define pYSIZ pYMAX-pYOFF

# define MAX_DEPTH 20
	long
x_board[MAX_DEPTH], y_board[MAX_DEPTH],
x_base[MAX_DEPTH], y_base[MAX_DEPTH];
	int
depth;
	char
overlay[200];

#define MAXARG 200
	char
plpath[80],
pscale[30] = ".2",
pxbase[30] = "0",
pybase[30] = "0",
plsize[30] = "-DR910,700",	/* proto */
	/* standard files created by szoom */
*povert,	/* text created by szoom */
*povero;	/* [p]legend output */
	char *
plotter[MAXARG] = {
	plpath,
	"-d",
	"sunzoom",
	"-s",
	pscale,
	"-X",
	pxbase,
	"-Y",
	pybase,
	plsize,
	0,
};
#define ARGBASE 11
	void
locexit(ret) {
	static char work[200] = "wc ";
	char name[100], *s;
	int lines =  0;

	strcat(work, povert);
	if (logfid) fclose(logfid);
	logfid = popen(work, "r");
	fscanf(logfid,"%d",&lines);
	pclose(logfid);
	while (lines > 1) {
		printf("name of file to save digitized data[destroy if null]");
		s = gets(name);
		while (*s && *s == ' ') ++s;
		if (!*s) {
			printf("are you sure?");
			s = gets(name);
			while (*s && *s == ' ') ++s;
			if (*s == 'y' || *s == 'Y')
				break;
			continue;
		}
		sprintf(work,"mv %s %s\n",povert,name);
		if (system(work) == 0)
			break;
		printf("transfer failed\n");
	}
	unlink(povero);
	unlink(povert);
	exit(ret);
}
	void
emess(code, s, s2) char *s, *s2; {

	fputs(s, stderr);
	if (s2) {
		fputc(' ', stderr);
		fputs(s2, stderr);
	}
	if (code == 2 || code == -2)
		perror();
	if (code > 0) {
		fputs("\nprogram abnomally terminated\n", stderr);
		locexit(1);
	}
	putc('\n', stderr);
	locexit(code);
}
# define MAX_LINE 150
# define MAXC 30
	void
setplot() { } /* don't need its services */
	static void
loaddef(fid) FILE *fid; {
	char line[MAX_LINE], *s, *argv[MAXC], *getline();
	int scalein();

	while (s = getline(line, MAX_LINE, fid)) {
		if (*s == '#') ++s;
		if (! words(s, MAXC, argv, scalein))
			return;
	}
	emess(1, "failed to scale",(char *)0);
	exit(11);
}
	int
loader(master) char *master; {
	FILE *fid;
	long tag;

	if (!(fid = fopen(master, "r"))) {
		emess(1, "master opening error, file:",master);
		exit(11);
	}
	if (fread(&tag, sizeof(long), 1, fid) != 1) {
		emess(1,"MAPGEN load error",(char *)0);
		exit(11);
	}
	rewind(fid);
	if (tag == MAGIC) {	/* MAPGEN */
		if (fread(&m_def, sizeof(m_def), 1, fid) != 1) {
			emess(1,"mapdef load lenth error",(char *)0);
			exit(11);
		}
		x_board[depth] = m_def.B.x_max;
		y_board[depth] = m_def.B.y_max;
		cts = m_def.cts_cm;
	} else {	/* PLOTGEN */
		loaddef(fid);
		x_board[depth] = def.x.board;
		y_board[depth] = def.y.board;
		cts = def.cts;
	}
	fclose(fid);
	sprintf(overlay,"%slegend %s %s -o %s",(tag==MAGIC?"":"p"),master,povert,povero);
}
static struct stat sbuf;
	int
main(argc, argv) char *argv[]; {
	int i, j;
	extern errno;
	char *usage="szoom def_file overlay[s]";
	char *getenv(), *s, *p, work[100];

	povero = tempnam((char*)0,"SO");
	povert = tempnam((char*)0,"SL");
	strcpy(work,"touch ");
	strcat(work,povero);
	system(work);
	plotter[ARGBASE-1] = povero;
	if (argc < 3) emess(1, "usage:", usage);
	loader(argv[1]);
	for (i = 2, j = ARGBASE; i < argc && j < MAXARG; ++i)
		plotter[j++] = argv[i];
	if (j+1 >= MAXARG) emess(1,"too many overlay files",(char *)0);
	if (!(s = getenv(GRAPHB)))
		emess(1,"graphics envirement not defined:",GRAPHB);
	for (p = plpath; (i = *s++) && i != ':'; ++p) *p = i;
	if (!(logfid = fopen(povert,"w")))
		emess(1,"log output open failure",(char *)0);
	if (sbuf.st_size == 0)
		fprintf(logfid," -f - -s .3 -l .5 # szoom default header line\n");
	s_core(argc, argv);
	locexit(0);
}
extern float x_size, y_size; 
float scale, bscale;
	void
doplot() {
	FILE *infile, *lpopen();
	double fx, fy;
	void lpclose();

	fflush(logfid);
	system(overlay);
	fx = x_size / x_board[depth];
	fy = y_size / y_board[depth];
	if (fx > fy) fx = fy;
	bscale = 1. / fx;
	scale = 1. / cts;
	sprintf(pscale,"%g",fx);
	sprintf(pxbase,"-%ld",x_base[depth]);
	sprintf(pybase,"-%ld",y_base[depth]);
	sprintf(plsize,"-DR%d,%d",(int)x_size,(int)y_size);
	if (!(infile = lpopen(plotter)))
		Bomb("plot open failure",10);
	splot(infile);
	lpclose(infile);
}
scprint(x, y) double x, y; {
	x = x * bscale + x_base[depth];
	y = y * bscale + y_base[depth];
	fprintf(logfid,"%.2f\t%.2f\n",x*scale,y*scale);
}
	int
zoompan(xm, ym, xt, yt, mode) double xm, ym, xt, yt; {
	int xl, yl, xh, yh, last;

	if (!mode && depth == 0) return(0);
	last = depth;
	if (mode && ++depth >= MAX_DEPTH) {
		depth = last;
		return(0);
	}
	if (mode && xm < xt) {
		xl = bscale * xm + x_base[last];
		xh = bscale * xt + x_base[last];
	} else {
		xl = bscale * xt + x_base[last];
		xh = bscale * xm + x_base[last];
	}
	if (mode && ym < yt) {
		yl = bscale * ym + y_base[last];
		yh = bscale * yt + y_base[last];
	} else {
		yl = bscale * yt + y_base[last];
		yh = bscale * ym + y_base[last];
	}
	if (xl == xh || yl == yh) {
		depth = last;
		return(0);
	}
	if (mode) {
		x_base[depth] = xl;
		y_base[depth] = yl;
		x_board[depth] = xh - xl;
		y_board[depth] = yh - yl;
	} else {
		if ((x_base[depth] += xh - xl) < 0) x_base[depth] = 0;
		if ((y_base[depth] += yh - yl) < 0) y_base[depth] = 0;
	}
	return(1);
}
	int
dezoom() {
	if (depth > 0) {
		--depth;
		return(1);
	} else
		return(0);
}
	int
homer() {
	if (depth > 0) {
		depth = 0;
		return 1;
	} else
		return 0;
}
/* long list form of popen */
#include <signal.h>

#define WRITE  1
#define READ   0

static int pltr_pid;

	FILE *
lpopen(argv) char *argv[]; {
	FILE *fp;
	int pi[2];

	if (pipe(pi) < 0)
		return ((FILE *)0);
	fflush(logfid);
	if ((pltr_pid = fork()) == 0) { /* new prog */
		close(1);
		dup(pi[WRITE]);
		close(pi[READ]);
		execv(argv[0], argv);
		fprintf(stderr,"can't execute prog:%s\n",
			argv[0]);
		_exit(1); /* disaster occurred */
	} /* else old program */
	if (pltr_pid == -1) {
		close(pi[READ]);
		close(pi[WRITE]);
		return ((FILE *)0);
	}
	close(pi[WRITE]);
	/* set to buffered I/O */
	fp = fdopen(pi[READ], "r");
	return (fp);
}
	void
lpclose(fp) FILE *fp; {
	int r;
	void (*hstat)(), (*istat)(), (*qstat)();
	int status;

	if (fp)
		fclose(fp);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
	while ((r = wait(&status)) != pltr_pid && r != -1) ;
	if (r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);
}
