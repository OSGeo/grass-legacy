#ifndef lint
static char *SCCSID = "@(#)TRparse.c	AMG v.3.2";
#endif
# include <stdio.h>
# include <ctype.h>
# include <search.h>
# include "TRdev.h"
# define VMAX 50
static ENTRY item, *fitem;
ENTRY *hsearch();
long strtol();
int v[VMAX];
	char
sbuf[20];
# define inchar() (--incnt < 0 ? getline() : (int)*inptr++)
# define MAXIN 250
	static char
inbuf[MAXIN],
*inptr;
	static int
incnt = -1,
linecnt = 0;
	static int
getline() {
	for (;;)
		if (inptr = fgets(inbuf, MAXIN, stdin)) {
			++linecnt;
			if (*inbuf == '!')
				userline(inbuf+1);
			else {
				incnt = strlen(inptr) - 1;
				return(*inptr++);
			}
		} else
			return(EOF);
}
	static void
unget(c) {
	if (c != EOF)
		if (incnt >= 0) {
			++incnt;
			*--inptr = c;
		} else {
			incnt = 0;
			inptr = inbuf;
			*inptr = c;
		}
}
	void
bomb(s) char *s; {
	fprintf(stderr,"<%s> on line %d\n",s,linecnt);
	exit(1);
}
	static int
getnum() {
	int c, num;

	if (!isdigit(c = inchar())) bomb("expecting digit Xn");
	for (num = c - '0'; isdigit(c = inchar()) ;
		num = num * 10 + c - '0') ;
	if (!isspace(c))
		unget(c);
	return(num);
}
	static void
xproc() {
	char str[100], *s;
	int c, i;

	while (isspace(c = inchar())) ;
	while (isgraph(*inptr)) ++inptr; /* swallow possible long name */
	switch (c) {
	case 'i':
		init(device.paperwidth, device.paperlength);
		break;
	case 'T':
		while (isspace(*inptr)) ++inptr;
		for (s = str; isgraph(*inptr); ) *s++ = *inptr++;
		*s = '\0';
		TRsetdev(str);
		break;
	case 'r':
		for (i = 0; i < 4 && *inptr && *inptr != '\n' ; ++i)
			v[i] = strtol(inptr, &inptr, 0);
		if (i != 3)
			bomb("improper r arglist");
		setres(v[0], v[1], v[2]);
		break;
	case 'p':
		pause();
		break;
	case 's':
		stop();
		break;
	case 't':
		trailer();
		break;
	case 'f':
		i = strtol(inptr, &inptr, 0);
		while (isspace(*inptr)) ++inptr;
		for (s = str; isgraph(*inptr); ) *s++ = *inptr++;
		*s = '\0';
		TRloadfont(i, str);
		break;
	case 'H':
		height((int)(strtol(inptr, &inptr, 0)));
		break;
	case 'S':
		slant((int)(strtol(inptr, &inptr, 0)));
		break;
	case 'U':
		while (isspace(*inptr)) ++inptr;
		useropt(inptr);
		break;
	default:
		bomb("unexpected 'x' arg");
		break;
	}
	incnt = 0; /* clear input line */
}
	static void
Dproc() {
	int c, i, d;

	c = inchar();
	for (i = 2; i < VMAX ; ++i) {
		while (isspace(d = *inptr)) ++inptr;
		if (isdigit(d) || d == '-')
			v[i] = strtol(inptr, &inptr, 0);
		else
			break;
	}
	if (i >= VMAX)
		bomb("overflowed D list");
	incnt = 0; /* mark line empty */
	v[0] = H;
	v[1] = V;
	switch (c) {
	case 'l':
		if (i != 4) bomb("Dl arg. len. error");
		line(v);
		H += v[2];
		V += v[3];
		break;
	case 'c':
		if (i != 3) bomb("Dc arg. len. error");
		circle(v);
		H += v[2];
		break;
	case 'e':
		if (i != 4) bomb("De arg. len. error");
		ellipse(v);
		H += v[2];
		break;
	case 'a':
		if (i != 6) bomb("Da arg. len. error");
		arc(v);
		H += v[2] + v[4];
		V += v[3] + v[5];
		break;
	case '~':
		if (i < 3 || (i & 1)) bomb("D~ arg. len. error");
		spline(v, (i+1)/2);
		H += v[i-1];
		V += v[i];
		break;
	default:
		bomb("unknow D argument");
		break;
	}
}
	void
TRparse() {
	char *s;
	int c, num;

	while ((c = inchar()) != EOF)
	switch (c) {
	case ' ': case '\n':
		break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
/* number and character sequence */
		num = (c - '0')*10;
		if (! isdigit(c = inchar())) bomb("expected num @ nnx");
		num += c - '0';
		if (!isprint(c = inchar())) bomb("non printable @ nnx");
		H += num;
		if (c -= ' ') TRprint(c);
		break;
/* simple character */
	case 'c':
		if (!isprint(c = inchar())) bomb("non graphic char");
		if (c -= ' ') TRprint(c);
		break;
/* interword space */
	case 'w':
		break;
	case 'C':
/* Generate special character */
		for (s = sbuf; isgraph(c = inchar()); ) *s++ = c;
		*s = '\0';
		item.key = sbuf;
		if (!(fitem = hsearch(item, FIND)))
			bomb("can't find special char");
		TRprint((int)(fitem->data));
		break;
/* point size */
	case 's':
		setsize(size = getnum());
		break;
/* font number */
	case 'f':
		font = fonttab[map[nfont = getnum()]];
		setfont(nfont);
		break;
/* absolute horizontal position */
	case 'H':
		H = getnum();
		break;
/* relative horizontal position */
	case 'h':
		H += getnum();
		break;
/* absolute vertical position */
	case 'V':
		V = getnum();
		break;
/* relative vertical position */
	case 'v':
		V += getnum();
		break;
/* page number */
	case 'p':
		newpage(getnum());
		break;
/* # comment and na b */
	case '#': case 'n':
		while ((c = inchar()) != '\n' && c != EOF) ;
		unget(c);
		break;
/* x ? ... */
	case 'x':
		xproc();
		break;
/* D ? ... */
	case 'D':
		Dproc();
		break;
/* whoops (?) */
	default:
		bomb("unknown control character");
		break;
	}
}
