#include "map.h"
struct place pole;
struct coord twist;

invert(a,b,x,y)
float a,b,*x,*y;
{
	float l,t;
	struct place g;
	l = pole.wlon.l;
	t = twist.l;
	exchsup(&pole.wlon,&twist);
	rotate(a,b,x,y);
	exchsup(&pole.wlon,&twist);
	supl(&twist);
}

rotate(a,b,x,y)
float a,b,*x,*y;
{
	struct place g;
	deg2rad(a,&g.nlat);
	deg2rad(b,&g.wlon);
	normalize(&g);
	*x = g.nlat.l/RAD;
	*y = g.wlon.l/RAD;
}

	float an,aw,bn,bw,an1,aw1,bn1,bw1,pn,pw;
main()
{
	float theta;
	for(;;){
	if(scanf("%f%f%f%f",&an,&aw,&bn,&bw)!=4)
		break;
	orient(an,aw,0.);
	rotate(bn,bw,&bn1,&bw1);
/*	printf("b %f %f\n",bn1,bw1);*/
	orient(an,aw,bw1);
	invert(0.,-90.,&pn,&pw);
/*	printf("p %f %f\n",pn,pw);*/
	orient(pn,pw,0.);
	rotate(an,aw,&an1,&aw1);
	rotate(bn,bw,&bn1,&bw1);
	theta = (aw1+bw1)/2;
/*	printf("a %f %f \n",an1,aw1);*/
	orient(pn,pw,theta);
	rotate(an,aw,&an1,&aw1);
	rotate(bn,bw,&bn1,&bw1);
	if(abs(aw1-bw1)>180)
		if(theta<0.) theta=+180;
		else theta =- 180;
	orient(pn,pw,theta);
	rotate(an,aw,&an1,&aw1);
	rotate(bn,bw,&bn1,&bw1);
	printf(" %.4f %.4f %.4f\n",pn,pw,theta);
	printf("A %.4f %.4f\n",an1,aw1);
	printf("B %.4f %.4f\n",bn1,bw1);
	orient(an,aw,0.);
	rotate(bn,bw,&bn1,&bw1);
	orient(an,aw,bw1);
	invert(0.,90.,&pn,&pw);
/*	printf("p %f %f\n",pn,pw);*/
	orient(pn,pw,0.);
	rotate(an,aw,&an1,&aw1);
	rotate(bn,bw,&bn1,&bw1);
	theta = (aw1+bw1)/2;
/*	printf("a %f %f \n",an1,aw1);*/
	orient(pn,pw,theta);
	rotate(an,aw,&an1,&aw1);
	rotate(bn,bw,&bn1,&bw1);
	if(abs(aw1-bw1)>180)
		if(theta<0.) theta =+180;
		else theta =- 180;
	orient(pn,pw,theta);
	rotate(an,aw,&an1,&aw1);
	rotate(bn,bw,&bn1,&bw1);
	printf(" %.4f %.4f %.4f\n",pn,pw,theta);
	printf("A %.4f %.4f\n",an1,aw1);
	printf("B %.4f %.4f\n",bn1,bw1);
}}

supl(coord)
struct coord *coord;
{
	coord->l = PI - coord->l;
	if(coord->l>=PI)
		coord->l =- 2*PI;
	coord->c = - coord->c;
}
exchsup(f,g)
struct coord *f,*g;
{
	exch(&f->l,&g->l);
	exch(&f->c,&g->c);
	exch(&f->s,&g->s);
	supl(f);
	supl(g);
}

exch(x,y)
float *x,*y;
{
	float t;
	t = *x;
	*x = *y;
	*y = t;
}
