# include <stdio.h>

FILE *TEK ;
float obotx = 0.;
float oboty = 0.;
float botx = 0.;
float boty = 0.;
float scalex = 1.;
float scaley = 1.;

int oloy = -1;
int ohiy = -1;
int ohix = -1;
int oextra = -1;

pt(x,y)
{
	float sqrt(), dx,dy,d;
	int hix,hiy,lox,loy,extra;
	int n;
	hix=(x>>7) & 037;
	hiy=(y>>7) & 037;
	lox = (x>>2)&037;
	loy=(y>>2)&037;
	extra=x&03+(y<<2)&014;
	dx = hix-ohix;
	dy = hiy-ohiy;
	d = sqrt(dx*dx + dy*dy);
	n = d/12.8 + 0.5;
	if(hiy != ohiy){
		putch(hiy|040);
		ohiy=hiy;
	}
	if(hix != ohix){
		if(extra != oextra){
			putch(extra|0140);
			oextra=extra;
		}
		putch(loy|0140);
		putch(hix|040);
		ohix=hix;
		oloy=loy;
	}
	else{
		if(extra != oextra){
			putch(extra|0140);
			putch(loy|0140);
			oextra=extra;
			oloy=loy;
		}
		else if(loy != oloy){
			putch(loy|0140);
			oloy=loy;
		}
	}
	putch(lox|0100);
	while(n--)putch(0);
}

extern float botx;
extern float boty;
extern float obotx;
extern float oboty;
extern float scalex;
extern float scaley;

xsc(xi)
{
	int xa;
	xa = (xi - obotx)*scalex +botx;
	return(xa);
}
ysc(yi)
{
	int ya;
	ya = (yi - oboty)*scaley +boty;
	return(ya);
}

putch(c)
{
	putc(c,TEK);
}
openpl()
{
	fflush (stdout) ;
	TEK = fdopen (fileno (stdout), "w") ;
}

closepl()
{
	putc('\r', TEK);
	putc('\n', TEK);
	fflush(TEK);
}
