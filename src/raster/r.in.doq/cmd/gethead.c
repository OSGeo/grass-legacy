#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "byte.h"
#define HD_SIZE 400
#define NAME 40
#define I2 2
#define I3 3
#define I6 6
#define D24 24
#define D12 12

struct POINTS {
	double x,y;
	};
void print_cord(nw,ne,sw,se)
struct POINTS *nw,*ne,*sw,*se;
{
/*
fprintf(stderr,"%lf , %lf \t %lf , %lf\n",nw->x,nw->y,ne->x,ne->y);
fprintf(stderr,"%lf , %lf \t %lf , %lf\n",sw->x,sw->y,se->x,se->y);
*/
}

void geticord(ptr,x,y)
char *ptr;
double *x, *y;
{
int i;
char buf[10];

for (i=0;i<I6;i++) {
        buf[i] = *(ptr++);
        }
buf[i] = (char)NULL;
*x = atol(buf);
for (i=0;i<I6;i++) {
        buf[i] = *(ptr++);
        }
buf[i] = (char)NULL;
*y = atol(buf);
}
 

void getres(ptr,x,y)
char *ptr;
double *x,*y;
{
int i;
double frac,pwr;
char buf[55], *cptr;
/********* Get X ***********/
for (i=0;i<D12;i++) { 
        buf[i] = *(ptr++); 
        } 
buf[i] = (char)NULL;
if ( buf[8] != 'E' ){
/*fprintf(stderr,"getres buf[8]!=E");
	fprintf(stderr,"Unknown format %c %s\n",buf[20],buf);
	exit();
*/
	sscanf(buf,"%lf",x);
	}
else {
	buf[8] = ' ';
	sscanf(buf,"%lf %lf",&frac,&pwr);
	if (pwr >= 0) 
		*x = frac * pow(10.0,pwr);
	else 
		*x = frac / pow(10.0,(pwr * -1.0));
	}

/*********** Get Y ***************/
for (i=0;i<D12;i++) { 
        buf[i] = *(ptr++); 
        } 
buf[i] = (char)NULL;
if ( buf[8] != 'E' ){
/*fprintf(stderr,"getres2 buf[8]!=E");
	fprintf(stderr,"Unknown format %c %s\n",buf[20],buf);
	exit();
*/
	sscanf(buf,"%lf",y);
	}
else {
	buf[8] = ' ';
	sscanf(buf,"%lf %lf",&frac,&pwr);
	if (pwr >= 0) 
		*y = frac * pow(10.0,pwr);
	else 
		*y = frac / pow(10.0,(pwr * -1.0));
	}
}

void getcord(ptr,x,y)
char *ptr;
double *x,*y;
{
int i;
double frac,pwr;
char buf[55], *cptr;
/********* Get X ***********/
for (i=0;i<D24;i++) { 
        buf[i] = *(ptr++); 
        } 
buf[i] = (char)NULL;
if ( buf[20] != 'D' ){
/*fprintf(stderr,"getcord buf[20]!=D");
	fprintf(stderr,"Unknown format %c %s\n",buf[20],buf);
	exit();
*/
	sscanf(buf,"%lf",x);
	}
else {
	buf[20] = ' ';
	sscanf(buf,"%lf %lf",&frac,&pwr);
	if (pwr >= 0) 
		*x = frac * pow(10.0,pwr);
	else 
		*x = frac / pow(10.0,(pwr * -1.0));
}

/*********** Get Y ***************/
for (i=0;i<D24;i++) { 
        buf[i] = *(ptr++); 
        } 
buf[i] = (char)NULL;
if ( buf[20] != 'D' ){
/*fprintf(stderr,"getcord2");
	fprintf(stderr,"Unknown format %c %s\n",buf[20],buf);
	exit();
*/
	sscanf(buf,"%lf",y);
	}
else {
	buf[20] = ' ';
	sscanf(buf,"%lf %lf",&frac,&pwr);
	if (pwr >= 0) 
		*y = frac * pow(10.0,pwr);
	else 
		*y = frac / pow(10.0,(pwr * -1.0));
	}
}


gethead (fp,cellhd,primary,name)
FILE *fp;
struct Cell_head *cellhd;
int primary;
char name[];
{
char *head, *r_buf, *cptr;
char buf[100];
double x_res,y_res,cx_res,cy_res;
int i,n,bands,p_datum,s_datum,order;
struct POINTS p,s,nwp,nws,swp,sws,nep,nes,sep,ses;
struct POINTS pnwi,pswi,pnei,psei;
struct POINTS snwi,sswi,snei,ssei;


/******************* Initialize header parts *****************/
cellhd->format = 0;
cellhd->compressed = 0;
cellhd->ew_res = 1.0;
cellhd->ns_res = 1.0;
/********* End Initialize **********************/

fgets(buf,90,fp);
if (strstr(buf,"BEGIN_USGS_DOQ_HEADER") != NULL) {
	printf("\n Using DOQ standard header\n");
	get_newhead(fp,cellhd,primary,name);
	}
else {
rewind(fp);
printf("\n Using OLD DOQ header\n");
head = G_malloc(HD_SIZE+10);
bytecount = 4;
if ((n=fread(head,1,HD_SIZE,fp)) != HD_SIZE) {
	fprintf(stderr,"can't read data %d\n",n);
	exit();
	}

cptr = head;
for (i=0;i<NAME;i++) {
	name[i] = *(cptr++) ;
	}
name[i]=(char)NULL;
printf("Working on Quadrangle %s\n",name);

/*** Check data ordering ******/
cptr = head + 141;
for (i=0;i<I3;i++) {
	buf[i] = *(cptr++);
	}
buf[i] = (char)NULL;
order = atoi(buf);
if (order != 2) {
	fprintf(stderr,"Order is %s\n",buf);
	exit();
	}

/********* Get rows and columns *************/
cptr = head + 144;  
for (i=0;i<I6;i++) { 
        buf[i] = *(cptr++); 
        } 
buf[i] = (char)NULL;
cellhd->rows = atoi(buf);
cptr = head + 150;  
for (i=0;i<I6;i++) { 
        buf[i] = *(cptr++); 
        } 
buf[i] = (char)NULL;
cellhd->cols = atoi(buf);
r_buf = G_malloc(cellhd->cols+10);

/*********** Get # bands now handlws one ***************/
cptr = head + 156;  
for (i=0;i<I3;i++) { 
        buf[i] = *(cptr++); 
        } 
buf[i] = (char)NULL;
bands = atoi(buf);
if (bands != 1){
	fprintf(stderr,"There are %d bands in this image.\n This routine presently handles on one\n");
	exit();
	}


/******* Get Datums ***************/
cptr = head + 167;  
for (i=0;i<I2;i++) { 
        buf[i] = *(cptr++); 
        } 
buf[i] = (char)NULL;
p_datum = atoi(buf);

cptr = head + 169;  
for (i=0;i<I2;i++) { 
        buf[i] = *(cptr++); 
        } 
buf[i] = (char)NULL;
s_datum = atoi(buf);



/************** Get projection *************/
cptr = head + 195;  
for (i=0;i<I6;i++) { 
        buf[i] = *(cptr++); 
        } 
buf[i] = (char)NULL;
cellhd->proj = atoi(buf);
if (cellhd->proj == 0) cellhd->proj = PROJECTION_LL;
if (cellhd->proj == 1) cellhd->proj = PROJECTION_UTM;
if (cellhd->proj == 2) cellhd->proj = PROJECTION_SP;

cptr = head + 198;  
for (i=0;i<I6;i++) { 
        buf[i] = *(cptr++); 
        } 
buf[i] = (char)NULL;
cellhd->zone = atoi(buf);
/*********** Get primary tick values ***************/
cptr = head + 207;
getcord(cptr,&swp.x,&swp.y);
cptr = head + 255;
getcord(cptr,&nwp.x,&nwp.y);
cptr = head + 303;
getcord(cptr,&nep.x,&nep.y);
cptr = head + 351;
getcord(cptr,&sep.x,&sep.y);

/************ Read to second record ***********/
i = cellhd->cols - HD_SIZE;
if (fread(r_buf,1,i,fp) != i) {
	fprintf(stderr,"can't read data2\n");
	exit();
	}

/******************* Read secondary cordinate values ***********/
if (fread(head,1,HD_SIZE,fp) != HD_SIZE){
	fprintf(stderr,"can't read data4\n");
	exit();
	}
cptr = head + 192;
getcord(cptr,&sws.x,&sws.y);
cptr = head + 240;
getcord(cptr,&nws.x,&nws.y);
cptr = head + 288;
getcord(cptr,&nes.x,&nes.y);
cptr = head + 336;
getcord(cptr,&ses.x,&ses.y);

 
/************ Read to third record ***********/
i = cellhd->cols - HD_SIZE;
if (fread(r_buf,1,i,fp) != i) {
	fprintf(stderr,"can't read data2\n");
	exit();
	}

 

/*********** Read header on third record ***************/
if (fread(head,1,HD_SIZE,fp) != HD_SIZE){
	fprintf(stderr,"can't read data4\n");
	exit();
	}
/************* Get internal coord SW and NE *************/
 
cptr = head + 192; 
geticord(cptr,&pswi.y,&pswi.x);
 
cptr = head + 204; 
geticord(cptr,&pnwi.y,&pnwi.x);
 
cptr = head + 216; 
geticord(cptr,&pnei.y,&pnei.x);
 
cptr = head + 228; 
geticord(cptr,&psei.y,&psei.x);
 
cptr = head + 240; 
geticord(cptr,&sswi.y,&sswi.x);
 
cptr = head + 252; 
geticord(cptr,&snwi.y,&snwi.x);
 
cptr = head + 264; 
geticord(cptr,&snei.y,&snei.x);
 
cptr = head + 278; 
geticord(cptr,&ssei.y,&ssei.x);
 

/************** Get ground coordinates of pixel 1,1 ***********/
cptr = head + 288;
getcord(cptr,&p.x,&p.y);
cptr = head + 336;
getcord(cptr,&s.x,&s.y);



/************ Read to fourth record ***********/
i = cellhd->cols - HD_SIZE;
if (fread(r_buf,1,i,fp) != i) {
	fprintf(stderr,"can't read data2\n");
	exit();
	}

/*********** Read header on fourth record ***************/
if (fread(head,1,HD_SIZE,fp) != HD_SIZE){
	fprintf(stderr,"can't read data4\n");
	exit();
	}


/*********** Get ground resolution *************/
cptr = head + 59;
getres(cptr,&x_res,&y_res);
/*
cx_res = (nes.x - nws.x) / (snei.x - snwi.x);
cy_res = (nws.y - sws.y) / (sswi.y - snwi.y);
*/
/********* Need to calculate this because of datum problems *******/
/************* Error in DOQ header *********** do DEBUG
print_cord(&nwp,&nep,&swp,&sep);
print_cord(&nws,&nes,&sws,&ses);
print_cord(&p,&s,&pnwi,&snwi);
fprintf(stderr,"x_res %lf y_res %lf cx_res %lf cy_res %lf\n",x_res,y_res,cx_res,cy_res);
cellhd->ew_res = x_res = cx_res;
cellhd->ns_res = y_res = cy_res;
*/
cellhd->ew_res = x_res;
cellhd->ns_res = y_res;
/*
cellhd->ew_res = x_res = 1.0000;
cellhd->ns_res = y_res = 1.0000;
*/

if (primary) {
	cellhd->north = nwp.y + (pnwi.y * y_res) - 0.5;
	cellhd->south = cellhd->north - (cellhd->rows * y_res);
	cellhd->west = nwp.x - (pnwi.x * x_res) + 0.5;
	cellhd->east = cellhd->west + (cellhd->cols * x_res);
	switch (p_datum) {
		case 1:
			printf("Primary datum NAD 27\n");
			break;
		case 2:
			printf("Primary datum WGS 72\n");
			break;
		case 3:
			printf("Primary datum WGS 84\n");
			break;
		case 4:
			printf("Primary datum NAD 83\n");
			break;
		case 5:
			printf("Primary datum Old Hawaii\n");
			break;
		case 6:
			printf("Primary datum Puerto Rico\n");
			break;
		}
	}
else {
	cellhd->north = nws.y + (snwi.y * y_res) - (1.5 * y_res);
	cellhd->south = cellhd->north - (cellhd->rows * y_res);
	cellhd->west = nws.x - (snwi.x * x_res) + (x_res/2.0);
	cellhd->east = cellhd->west + (cellhd->cols * x_res);
	switch (s_datum) {
		case 1:
			printf("Secondary datum NAD 27\n");
			break;
		case 2:
			printf("Secondary datum WGS 72\n");
			break;
		case 3:
			printf("Secondary datum WGS 84\n");
			break;
		case 4:
			printf("Secondary datum NAD 83\n");
			break;
		case 5:
			printf("Secondary datum Old Hawaii\n");
			break;
		case 6:
			printf("Secondary datum Puerto Rico\n");
			break;
		}
	}

/************ Read to fifth record ***********/
i = cellhd->cols - HD_SIZE;
if (fread(r_buf,1,i,fp) != i) {
	fprintf(stderr,"can't read data2\n");
	exit();
	}

return(1);
}
}
