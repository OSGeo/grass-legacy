#ifndef lint
static char *SCCSID = "@(#)Dc1077.c	OEMG v.3.2";
#endif
/*	Calcomp 906 et al output
**		model_no = 0 mechanical
**			 = 1 electrostatic color
**	for UNIX V only.
**	for simplicity's sake this is radix 64 version
*/
# include <varargs.h>
# include "plotter.h"
# include <fcntl.h>
# include <termio.h>

# define MAX_MODEL 2
/* model variations: */
static struct {
	int	XPMAX, YPMAX, PENS;
} Model[MAX_MODEL] = {
	400000., 69000., 4,
	400000., 69000., 4,
};
# define BIAS		' '

# ifdef DEBUG
# define EOB		'\n'
# define SYNC		'?'
# else
# define EOB		3
# define SYNC		2
# endif

# define C_PENUP	3
# define C_PENDOWN	2
# define SEL_PEN	4
# define OK		'0'
# define BAD		'1'
# define REQ		'>'

	static long sumx, sumy, maxx=0;
	extern long
lrnd();
	static XYS
cursor = { 0, 0, "\0" };
	static long
fxls,
fyls;
	static int
pendown,
oldpen;
	static char
nulls[] = { ' ',' ',' ',' ',' ', ' ',' ',' ',' ',' ', ' ',' ',' ',' ',' ', ' ',' ',' ',' ',' '};
# define ESC(n)	8, n
# define DBL(n) ((n)>>4)&0xf, (n)&0xf
	static char
set[] = {
	1, 0, 0, 1,	/* search address */
	7, 63,		/* radix - 1 */
	ESC(10),	/* 16 - 128 byte buffers */
	ESC(2), 0,	/* set response suffix length */
	ESC(3), 0,	/* set turnaround delay */
	ESC(4),	1, DBL(OK),	/* set good response message */
	ESC(5), 1, DBL(BAD),	/* set bad response message */
	ESC(6), 1, DBL(REQ),	/* set response request string */
	4, 1,		/* pen select */
	9, 1		/* scaling code */
};
	static char
d_code[][7] = {
	0x13, 0x2b, 0x2f, 0x1f, 0x2e, 0x2a, 0x12,
	0x33, 0x17, 0x3b, 0x23, 0x3a, 0x16, 0x32,
	0x37, 0x3f, 0x1b, 0x27, 0x1a, 0x3e, 0x36,
	0x1d, 0x21, 0x25, 0x00, 0x26, 0x22, 0x1e,
	0x35, 0x3d, 0x19, 0x24, 0x18, 0x3c, 0x34,
	0x31, 0x15, 0x39, 0x20, 0x38, 0x14, 0x30,
	0x11, 0x29, 0x2d, 0x1c, 0x2c, 0x28, 0x10
};
/**********************************
** file handling section
***********************************/
	static int
fildes = 1;
	static FILE
*file = stdout;
# define MAX_RETRY 5

# ifdef DEBUG
# define BUFFSIZE 70
	static struct termio
tiob;
# else
# define BUFFSIZE 129
# endif

# define LEN_PRE 2
# define PRELUDE SYNC, BIAS
# define DATASIZE BUFFSIZE - LEN_PRE - 2
	static char
ans = REQ,
buf1[BUFFSIZE] = {PRELUDE},
buf2[BUFFSIZE] = {PRELUDE};
static struct {
	int	len;
	char	*buffer;
} bufs[2] = { 0, buf1, 0, buf2};
	static char
*b_ptr = buf1 + LEN_PRE;
	static int
b_sum,
b_cnt = DATASIZE;
	static long
record;
# define check(n) ((n) > b_cnt ? bflush(): 0)
	static	/* flush buffer if less than 'n' free left */
bflush() {
	int retry, rec;
	char chk;

	if (!file && record) { /* check last transmission */
		rec = (record - 1) & 1;
		for (retry = MAX_RETRY; retry ; --retry) {
			write(fildes, &ans, 1);
			while (read(fildes, &chk, 1) &&
				chk != OK && chk != BAD)  ;
			if (chk == OK)
				break;
			else if (chk == BAD) {
				write(fildes, bufs[rec].buffer,
					bufs[rec].len);
/*
			write(fildes, nulls, sizeof(nulls));
*/
			}
		}
		if (!retry) {
			fprintf(stderr,"failed on retries\n");
			closer();
			exit(1);
		}
	}
	if (bufs[rec = record & 1].len = DATASIZE - b_cnt) {
		*b_ptr++ = 96 - (b_sum & 0x1f);
		*b_ptr = EOB;
		bufs[rec].len += LEN_PRE + 2;
		if (file)
			fwrite(bufs[rec].buffer, bufs[rec].len, 1, file);
		else {
			write(fildes, bufs[rec].buffer, bufs[rec].len);
			if (!record) {
				static char com[]="SHIT ** PLOTTER BEGGINS\r\n";
				write(fildes, com, sizeof(com));
/*
				write(fildes, nulls, sizeof(nulls));
*/
			}
		}
		record++;
		b_sum = 0;
		b_ptr = bufs[record & 1].buffer + LEN_PRE;
		b_cnt = DATASIZE;
	}
}
	static	/* put biased character into buffer */
bputc(c) {
	if (!b_cnt)
		bflush();
	b_cnt--;
	*b_ptr++ = (c += BIAS);
	b_sum += c;
}
	static struct termio
tio;
	static
opener() {
	char *name, *ttyname();

	if (isatty(fileno(file))) { /* is this a tty file? */
			/* yes, so let's treat as direct i/o */
		name = ttyname(fileno(file));
		fclose(file);
		file = (FILE *)0;
		if ((fildes = open(name, O_RDWR)) < 0)
			return(0);
			/* set up dev control */
		if (ioctl(fildes, TCGETA, &tio) < 0) {
			closer();
			return(0);
		}
/* terminal control **************************************/
# ifdef DEBUG
		tiob = tio;
		tio.c_cflag = EXTA+CS8+CREAD;
# else
		tio.c_cflag = B9600+CS8+CREAD;
# endif
		tio.c_iflag = ISTRIP+IGNPAR;
		tio.c_oflag = 0;
		tio.c_lflag = 0;
		tio.c_cc[VTIME] = 30;
		tio.c_cc[VMIN] = 1;
/*********************************************************/
		ioctl(fildes, TCSETA, &tio);
	}
	return(1); /* Go babes! */
}
	static
closer() {
	if (file)
		fclose(file);
	else {
# ifdef DEBUG
		ioctl(fildes, TCSETA, &tiob);
# endif
		close(fildes);
	}
}
/**********************************
** end file handling section
**********************************/
	static
cvt ( z, s ) long z; char *s; {
	register i;
	register long v;

	if (!(v = z < 0 ? -z : z))
		return (0);
	for ( i = 0; v ; i++ ) {
		*s++ = (v & 077);
		v >>= 6;
	}
	return (z < 0 ? -i : i);
}
	static
delta(x, y) long x, y; {
	static char sx[3], sy[3];
	register ix, iy, c;

	ix = cvt(x, sx);
	iy = cvt(y, sy);
	c = d_code[iy + 3][ix + 3];
	ix = abs(ix);
	iy = abs(iy);
	check(ix + iy + 1);
	bputc(c);
	while (ix) bputc(sx[--ix]);
	while (iy) bputc(sy[--iy]);
}
	static
mkreset() {
	long adx,ady,dx,dy;
	int t;
	double r;

	bputc(C_PENUP);
	if (Dglobal.model_no == 1) {
		adx = sumx; ady = sumy;
		dx = -adx; dy = -sumy;
	} else {
		dx = adx = maxx - sumx + 800; ady = sumy; dy = -ady;
	}
/* DEBUG  fprintf(stderr,"mod=%d, x=%ld, y=%ld\n",Dglobal.model_no,dx,dy);*/
	if (adx > 32767 || ady > 32767) 
		for (t = (adx > ady ? adx : ady) / 32767; t ; --t) {
			r = 1. / (t + 1);
			adx = lrnd(dx * r);
			ady = lrnd(dy * r);
			delta(adx, ady);
			dx -= adx;
			dy -= ady;
		}
	if (dx || dy)
		delta(dx, dy);	
}	
		
	XYS *
Dc1077(va_alist) va_dcl {
	va_list vap;
	double r;
	long adx, ady, dx, dy;
	int cmd, i, t, pen;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch (cmd) {

	case D_SCALE:
		if ((Dglobal.scale *= 2.) <= 0.) Dglobal.scale = 2.;
		goto scaleit;
	case D_INIT:
		if (!opener()) {
			ret = (XYS *)0;
			break;
		}
		for (i = 0; i < sizeof(set); ++i)
			bputc(set[i]);
		bflush();
/************ ADD Nulls ************************/
                if (!file) write(fildes, nulls, sizeof(nulls));
                else fwrite(nulls, sizeof(nulls), 1, file);
/*********** END of ADD ***/

		pendown = oldpen = 0;
		fxls = fyls = 0;
		Dglobal.scale *= 2.;
scaleit:	cursor.x = Model[Dglobal.model_no].XPMAX / Dglobal.scale;
		cursor.y = Model[Dglobal.model_no].YPMAX / Dglobal.scale;
		break;
	case D_DONE:
		mkreset();
		bflush();
		closer();
	case D_PANIC:
		break;
	case D_MOVE:
		if (pendown) {
			pendown = 0;
			bputc(C_PENUP);
		}
		goto moveit;
	case D_LINE:
		if (!pendown) {
			pendown++;
			bputc(C_PENDOWN);
		}
moveit:
		ady = abs(dy = lrnd((va_arg(vap, long)) *
			Dglobal.scale) - fyls);
		adx = abs(dx = lrnd((va_arg(vap, long)) *
			Dglobal.scale) - fxls);
		if ((sumx += dx) > maxx) maxx = sumx;
		sumy += dy;
		fxls += dx;
		fyls += dy;
		if (adx > 32767 || ady > 32767) /* bigger'n 32767? */
			for (t = (adx > ady ? adx : ady) / 32767; t ; --t) {
				r = 1. / (t + 1);
				adx = lrnd(dx * r);
				ady = lrnd(dy * r);
				delta(adx, ady);
				dx -= adx;
				dy -= ady;
			}
		if (dx || dy)
			delta(dx, dy);
		break;
	case D_PEN:
/*		pen = (va_arg(vap, long)) % Model[Dglobal.model_no].PENS;*/
		while ((pen = (va_arg(vap, long))) > Model[Dglobal.model_no].PENS)
			pen -= Model[Dglobal.model_no].PENS;
		if (pen == 0) pen++;
		if (pen != oldpen) {
			check(2);
			bputc(SEL_PEN); bputc(pen);
			oldpen = pen;
			pendown = 0;
		}
		break;
	}
	va_end(vap);
	return ret;
}
