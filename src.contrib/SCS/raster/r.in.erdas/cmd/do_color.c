#include "gis.h"

static void convchar(sbuf,dbuf)
char sbuf[];
int dbuf[];
{
int i;

for (i=0;i<256;i++) 
	if (sbuf[i] < 0) dbuf[i] = sbuf[i] + 256;
	else dbuf[i] = sbuf[i];
}

/******************* main routine ********************/

do_color(trl,map,band)
char trl[],map[];
int band;

{

int erdf, i, n;

char buff[256];
unsigned int green[256];
unsigned int red[256];
unsigned int blue[256];
struct Colors colors;



if ((erdf = open(trl,0)) < 0) {
	fprintf(stderr,"Error can not open ERDAS trailer file\n");
	exit(0);
	}

G_init_colors (&colors);

/**************** READ HEAD ********************/
n = read(erdf,buff,128);
if (n!=128) {
	fprintf(stderr,"Error in reading trailer file\n");
	exit(0);
	}

/************ READ GREEN values *********************/
n = read(erdf,buff,256);
if (n!=256) {
	fprintf(stderr,"Error in reading trailer file\n");
	exit(0);
	}
convchar(buff,green);
/************ READ RED values *********************/
n = read(erdf,buff,256);
if (n!=256) {
	fprintf(stderr,"Error in reading trailer file\n");
	exit(0);
	}
convchar(buff,red);
/************ READ BLUE values *********************/
n = read(erdf,buff,256);
if (n!=256) {
	fprintf(stderr,"Error in reading trailer file\n");
	exit(0);
	}
convchar(buff,blue);
for (i=0;i<256;i++) {
	G_set_color (i, red[i], green[i], blue[i], &colors);
	}
sprintf(buff,"%s.%d",map,band);
G_write_colors (buff, G_mapset(), &colors);
}
