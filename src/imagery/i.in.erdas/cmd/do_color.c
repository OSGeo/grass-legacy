static char rcsid[]="$Header: /usr3/4.0/src.contrib/SCS/raster/r.in.erdas/cmd/RCS/do_color.c,v 1.3 1992/06/27 02:06:51 grass Exp $";
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
close(erdf);
}

do_label(trl,map,band,nclass)
char trl[],map[];
int band,nclass;

{

int erdf, i, n;

char *buff, tmpbuf[50],tmpbuf1[50];
struct Categories cats;

if ((erdf = open(trl,0)) < 0) {
	fprintf(stderr,"Error can not open ERDAS trailer file\n");
	exit(0);
	}

G_init_cats ( (CELL)nclass, "", &cats);
if (nclass*32 < 2048)
	buff=G_malloc(2048);
else	
	buff=G_malloc(nclass*32);

/**************** READ HEAD ********************/
n = read(erdf,buff,2048);
if (n!=2048) {
	fprintf(stderr,"Error in reading trailer file header\n");
	exit(0);
	}

/* get title from trailer file header */
G_strncpy(tmpbuf, buff+72, 44);
n = strcspn(tmpbuf, "~");
G_strncpy(tmpbuf1, tmpbuf, n);
G_set_cats_title(tmpbuf, &cats);

/************ READ LABEL values *********************/
n = read(erdf,buff,nclass*32);
if (n != nclass*32) {
	fprintf(stderr,"Error in reading trailer file labels\n");
	exit(0);
	}
for (i=0;i<nclass;i++) {
	G_strncpy(tmpbuf, buff+i*32, 32);
	n = strcspn(tmpbuf, "~");
	G_strncpy(tmpbuf1, tmpbuf, n);
	G_set_cat (i, tmpbuf1, &cats);
	}
sprintf(buff,"%s.%d",map,band);
G_write_cats (buff, &cats);
G_free_cats(&cats);
free(buff);
close(erdf);
}
