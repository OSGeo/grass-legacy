#include "map.h"
#define NTRACK 10

int (*projection)();
float atof(), floor(), ceil();
int azequidistant();
int mercator();
int cylindrical();
int rectangular();
int orthographic();
int sinusoidal();
int azequalarea();
int stereographic();
int gnomonic();
int perspective();
int cylequalarea();
int conic();
int polyconic();
int bonne();
int lambert();
int albers();
struct {
	char *name;
	int (*prog)();
	int npar;
	int cut;
} index[] {
	"azequidistant",
	azequidistant,
	0,
	0,
	"mercator",
	mercator,
	0,
	1,
	"cylindrical",
	cylindrical,
	0,
	1,
	"rectangular",
	rectangular,
	0,
	1,
	"orthographic",
	orthographic,
	0,
	0,
	"sinusoidal",
	sinusoidal,
	0,
	1,
	"azequalarea",
	azequalarea,
	0,
	0,
	"stereographic",
	stereographic,
	0,
	0,
	"gnomonic",
	gnomonic,
	0,
	0,
	"perspective",
	perspective,
	1,
	0,
	"cylequalarea",
	cylequalarea,
	1,
	1,
	"conic",
	conic,
	1,
	1,
	"polyconic",
	polyconic,
	0,
	1,
	"bonne",
	bonne,
	1,
	1,
	"lambert",
	lambert,
	2,
	1,
	"albers",
	albers,
	2,
	1,
	0
};

int cut;
float orientation[3] { 90., 0., 0. };
int delta 1;
float limits[4] {
	-90., 90., -180., 180.
};
float lolat, hilat, lolon, hilon;
float window[4] {
	-90., 90., -180., 180.
};
float params[2];
float xmin 100.;
float xmax -100.;
float ymin 100.;
float ymax -100.;
float xrange, yrange;
int left,right,bottom,top;
int sflag;
int mflag;
float grid[2] { 10., 10. };
float scale;
struct track {
	int tracktyp;
	char *tracknam;
} track[NTRACK];
int ntrack;

main(argc,argv)
char **argv;
{
	extern int fout;
	int i,j,k;
	int conn;
	char *s, *t;
	float x, y, lat, lon;
	int ix, iy;
	struct place g;
	if(argc<=1) 
		error("usage: map projection params options");
	for(k=0;index[k].name;k++) {
		s = index[k].name;
		t = argv[1];
		while(*s == *t){
			if(*s==0) goto found;
			s++;
			t++;
		}
	}
	printf("projections:\n");
	for(i=0;index[i].name;i++) 
		printf("%s\n",index[i].name);
	exit();
found:
	fout = 1;
	argv =+ 2;
	argc =- 2;
	cut = index[k].cut;
	for(i=0;i<index[k].npar;i++) {
		if(i>=argc||option(argv[i])) {
			printf("%s needs %d params\n",index[k].name,index[k].npar);
			exit();
		}
		params[i] = atof(argv[i]);
	}
	argv =+ i;
	argc =- i;
	while(argc>0&&option(argv[0])) {
		argc--;
		argv++;
		switch(argv[-1][1]) {
		case 'm':
			mflag++;
			break;
		case 'g':
			for(i=0;i<2&&argc>i&&!option(argv[i]);i++) {
				grid[1] = grid[i] = atof(argv[i]);
			}
			if(i==0)
				grid[0] = grid[1] = 0.;
			argc =- i;
			argv =+ i;
			break;
		case 't':
		case 'u':
			for(i=0;ntrack<NTRACK&&argc>i&&!option(argv[i]);i++) {
				track[ntrack].tracktyp = argv[-1][1];
				track[ntrack++].tracknam = argv[i];
			}
			argc =- i;
			argv =+i;
			break;
		case 's':
			sflag++;




			break;
		case 'o':
			for(i=0;i<3&&i<argc&&!option(argv[i]);i++)
				orientation[i] = atof(argv[i]);
			argv =+ i;
			argc =- i;
			break;
		case 'l':
			for(i=0;i<argc&&i<4&&!option(argv[i]);i++)
				limits[i] = atof(argv[i]);
			argv =+ i;
			argc =- i;
			break;
		case 'd':
			if(argc>0&&!option(argv[0])) {
				delta = atoi(argv[0]);
				argv++;
				argc--;
			}
			break;
		case 'w':
			for(i=0;i<argc&&i<4&&!option(argv[i]);i++)
				window[i] = atof(argv[i]);
			argv =+ i;
			argc =- i;
			break;
		}
	}
	if(argc>0)
		error("error in arguments");
	window[0] =- .01;
	window[1] =+ .01;
	window[2] =- .01;
	window[3] =+ .01;
	if(
		window[0]>=window[1]||
		window[2]>=window[3]||
		window[0]>90.||
		window[1]<-90.||
		window[2]>180.||
		window[3]<-180.)
		error("unreasonable window");
	for(i=0;i<4;i++)
		window[i] =* RAD;
	orient(orientation[0],orientation[1],orientation[2]);
	projection = (*index[k].prog)(params[0],params[1]);
	if(projection == 0)
		error("unreasonable parameters");
	lolat = floor(limits[0]/10)*10;
	hilat = ceil(limits[1]/10)*10;
	lolon = floor(limits[2]/10)*10;
	hilon = ceil(limits[3]/10)*10;
	if(lolon>hilon)
		hilon =+ 360.;
	if(lolon>=hilon||lolat>=hilat||lolat<-90.||hilat>90.)
		error("unreasonable limits");
	for(lat=lolat;lat<=hilat;lat=+10.) {
		for(lon=lolon;lon<=hilon;lon=+10.) {
			if(normproj(lat,lon,&x,&y)<=0)
				continue;
			if(x<xmin) xmin = x;
			if(x>xmax) xmax = x;
			if(y<ymin) ymin = y;
			if(y>ymax) ymax = y;
		}
	}
	xrange = xmax - xmin;
	yrange = ymax - ymin;
	if(xrange/4096>yrange/3210)
		scale = 4096/(xrange*1.1);
	else
		scale = 3210/(yrange*1.1);
/*
printf("minmax %.2f %.2f %.2f %.2f\n",xmin,xmax,ymin,ymax);
*/
	left = (xmin-.05*xrange)*scale;
	bottom = (ymin-.05*yrange)*scale;
	right = left + 4096;
	top = bottom + 3210;
	openpl();
	space(left,bottom,left+4096,bottom+4096);
	if(!sflag)
		erase();
	linemod("dot");
	if(grid[0]>0.) for(lat=ceil(lolat/grid[0])*grid[0];
			lat<=hilat;lat=+grid[0]) {
		conn = 0;
		for(lon=lolon;lon<=hilon;lon=+2) {
			deg2rad(lat,&g.nlat);
			deg2rad(lon,&g.wlon);
			conn = plotpt(&g,conn);
		}
	}
	if(grid[1]>0.) for(lon=ceil(lolon/grid[1])*grid[1];
			lon<=hilon;lon=+grid[1]) {
		conn = 0;
		for(lat=lolat;lat<=hilat;lat=+2) {
			deg2rad(lat,&g.nlat);
			deg2rad(lon,&g.wlon);
			conn = plotpt(&g,conn);
		}
	}
	linemod("solid");
	if(!mflag)
		data();
	for(i=0;i<ntrack;i++)
		satellite(&track[i]);

	move(right,bottom);
	closepl();
}


normproj(lat,lon,x,y)
float lat,lon;
float *x, *y;
{
	int i;
	struct place geog, map;
	deg2rad(lat, &geog.nlat);
	deg2rad(lon, &geog.wlon);
/*
	printp(&geog);
*/
	normalize(&geog);
	if(!inwindow(&geog))
		return(-1);
	i = (*projection)(&geog,x,y);
/*
	printp(&geog);
	printf("%d %.3f %.3f\n",i,*x,*y);
*/
	return(i);
}

inwindow(geog)
struct place *geog;
{
	if(geog->nlat.l<window[0]||
	   geog->nlat.l>window[1]||
	   geog->wlon.l<window[2]||
	   geog->wlon.l>window[3])
		return(0);
	else return(1);
}

option(s) 
char *s;
{

	if(s[0]=='-' && (s[1]<'0'||s[1]>'9') &&s[1]!='.')
		return(1);
	else
		return(0);
}

/*
openpl(){}
closepl(){}
erase(){}
point(ix,iy)
{
	printf("point %d %d\n",ix,iy);
}
space(x0,y0,x1,y1)
{
	printf("space %d %d %d %d\n",x0,y0,x1,y1);
}
*/

struct {int hi, lo; };
int buf[259];
long patch[18][36];

data()
{
	int kx,ky;
	int k;
	int b;
	int ip, jp;
	int n;
	struct place g;
	long byte;
	int i, j;
	float x, y;
	float lat, lon;
	int conn;

	close(0);
	if(open("usr/dict/world.x",0)!=0)
		error("can't find map index");
	if(fopen("usr/dict/world",buf)<0)
		error("can't find map data");

	while( scanf("%d%d%ld",&i,&j,&byte) == 3 )
		patch[i+9][j+18] = byte;
	for(lat=lolat;lat<hilat;lat=+10.)
		for(lon=lolon;lon<hilon;lon=+10.) {
			if(!(
				normproj(lat,lon,&x,&y)>0||
				normproj(lat+10,lon,&x,&y)>0||
				normproj(lat,lon+10,&x,&y)>0||
				normproj(lat+10,lon+10,&x,&y)>0))
				continue;
			i = pnorm(lat);
			j = pnorm(lon);
			if(patch[i+9][j+18].lo&1)
				continue;
			b = patch[i+9][j+18]/512;
			seek(buf[0],b,3);
			b = patch[i+9][j+18]%512;
			seek(buf[0],b,1);
			buf[1] = 0;
			while((ip=getc(buf))>=0&&(jp=getc(buf))>=0){
				if(ip!=(i&0377)||jp!=(j&0377))
					break;
				n = getw(buf);
				conn = 0;
				for(k=0;k<n;k++){
					kx = getw(buf);
					ky = getw(buf);
					if(k%delta!=0&&k!=n-1)
						continue;
					conv(kx,&g.nlat);
					conv(ky,&g.wlon);
					conn = plotpt(&g,conn);
				}
			}
		}
}

satellite(t)
struct track *t;
{
	int conn;
	float lat,lon;
	struct place place;
	cclose(0);
	if(copen(t->tracknam,0) < 0)
		error("can't find track");
	linemod(t->tracktyp=='t'?"dotdash":"solid");
	conn = 0;
	while(scanf("%f%f",&lat,&lon)==2){
		if(lat>90.) {
			conn = 0;
			continue;
		}
		deg2rad(lat,&place.nlat);
		deg2rad(lon,&place.wlon);
		if(limits[0]*RAD>place.nlat.l||
		   limits[1]*RAD<place.nlat.l||
		   limits[2]*RAD>place.wlon.l||
		   limits[3]*RAD<place.wlon.l) {
			conn = 0;
			continue;
		}
		conn = plotpt(&place,conn);
	}
}

pnorm(x)
float x;
{
	int i;
	i = x/10.;
	i =% 36;
	if(i>=18) return(i-36);
	if(i<-18) return(i+36);
	return(i);
}

conv(k,g)
struct coord *g;
{
	g->l = .0001*k;
	SinCos(g);
}

error(s)
char *s;
{
	closepl();
	printf(2,"\r\n%s\n",s);
	exit();
}

int oldx, oldy;

cpoint(xi,yi,conn)
{
	if(xi<left||xi>=right)
		return(0);
	if(yi<bottom||yi>=top)
		return(0);
	if (conn)
	{	line( oldx, oldy, xi, yi );
		oldx = xi;
		oldy = yi;
	}
	else
	{	oldx = xi;
		oldy = yi;
	}
	return(1);
}

plotpt(g,conn)
struct place *g;
{
	int kx,ky;
	float xs, ys;
	float x,y;
	static float olon;
	normalize(g);
	if(!inwindow(g))
		return(0);
	if(cut && conn && abs(g->wlon.l - olon)>=PI)
		conn = 0;
	olon = g->wlon.l;
	if((*projection)(g,&x,&y)<=0)
		return(0);
	kx = x*scale;
	ky = y*scale;
	return(cpoint(kx,ky,conn));
}

intabs(x)
{
	return(x>=0 ? x : -x);
}

