#include <stdio.h>

double convert(d,m,s)
int d, m;
double s;
{
double deg, dd, dm;
dd = d;
dm = m;
deg = dd + ( dm + s/60.0)/ 60.0 ;
return(deg);
}


main()
{
char dumb[10];
int wd,wm,ed,em,sd,sm,nd,nm;
double ws,es,ss,ns, w, e, s, n;


scanf("%dd%d'%lf%s %dd%d'%lf%s %dd%d'%lf%s %dd%d'%lf%s",&wd,&wm,&ws,dumb,&ed,&em,&es,dumb,&sd,&sm,&ss,dumb,&nd,&nm,&ns,dumb);


w = convert(wd,wm,ws);
e = convert(ed,em,es);
s = convert(sd,sm,ss);
n = convert(nd,nm,ns);

w = w + (e - w)/2;
s = s + (n - s)/2;

printf("%.3lf %.3lf\n",w,s);
}

