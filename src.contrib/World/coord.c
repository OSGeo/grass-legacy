#include "map.h"

struct place pole;	/* map pole is tilted to here */
struct coord twist;	/* then twisted this much */

orient(lat, lon, theta)
float lat, lon, theta;
{
	deg2rad(lat, &pole.nlat);
	deg2rad(lon, &pole.wlon);
	deg2rad(theta, &twist);
}

deg2rad(theta, coord)
float theta;
struct coord *coord;
{
	while(theta>180.)
		theta =- 360;
	while(theta<-180.)
		theta =+ 360.;
	coord->l = theta*RAD;
	if(theta==90) {
		coord->s = 1;
		coord->c = 0;
	} else if(theta== -90) {
		coord->s = -1;
		coord->c = 0;
	} else
		SinCos(coord);
}

SinCos(coord)
struct coord *coord;
{
	coord->s = sin(coord->l);
	coord->c = cos(coord->l);
}

normalize(gg)
struct place *gg;
{
	register struct place *g;	/*geographic coords */
	struct place m;			/* standard map coords*/
	register int i;
	struct { float x[6]; };
	g = gg;
	if(pole.nlat.s == 1.) {
		if(pole.wlon.l+twist.l == 0.)
			return;
		g->wlon.l =- pole.wlon.l+twist.l;
	} else {
		if(pole.wlon.l != 0) {
			g->wlon.l =- pole.wlon.l;
			SinCos(&g->wlon);
		}
		m.nlat.s = pole.nlat.s * g->nlat.s
			+ pole.nlat.c * g->nlat.c * g->wlon.c;
		m.nlat.c = sqrt(1. - m.nlat.s * m.nlat.s);
		m.nlat.l = atan2(m.nlat.s, m.nlat.c);
		m.wlon.s = g->nlat.c * g->wlon.s;
		m.wlon.c = pole.nlat.c * g->nlat.s
			- pole.nlat.s * g->nlat.c * g->wlon.c;
		m.wlon.l = atan2(m.wlon.s, - m.wlon.c)
			- twist.l;
		for(i=0;i<6;i++)
			g->x[i] = m.x[i];
	}
	SinCos(&g->wlon);
	if(g->wlon.l>PI)
		g->wlon.l =- 2*PI;
	else if(g->wlon.l<-PI)
		g->wlon.l =+ 2*PI;
}

double abs(x)
double x;
{
	return(x<0.? -x : x);
}

/*
double tan(x)
double x;
{
	return(sin(x)/cos(x));
}
*/
printp(g)
struct place *g;
{
printf("%.3f %.3f %.3f %.3f %.3f %.3f\n",
g->nlat.l,g->nlat.s,g->nlat.c,g->wlon.l,g->wlon.s,g->wlon.c);
}
