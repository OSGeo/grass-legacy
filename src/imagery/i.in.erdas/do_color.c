static char rcsid[]="$Header$";
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"

static void convchar (char sbuf[], unsigned int dbuf[])
{
	int i;

	for (i=0;i<256;i++) 
		dbuf[i] = ((unsigned char *) sbuf)[i];
}


int do_color (char trl[], char map[], int band)
{

int erdf, i, n;

char buff[256];
unsigned int green[256];
unsigned int red[256];
unsigned int blue[256];
struct Colors colors;



if ((erdf = open(trl,O_RDONLY)) < 0) {
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

    return 0;
}

int do_label (char trl[], char map[], int band, int nclass)
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
G_free(buff);
close(erdf);

    return 0;
}
