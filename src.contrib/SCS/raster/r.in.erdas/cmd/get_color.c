#include <stdio.h>


static void convchar(sbuf,dbuf)
char sbuf[];
int dbuf[];
{
int i;

for (i=0;i<256;i++) 
	if (sbuf[i] < 0) dbuf[i] = sbuf[i] + 256;
	else dbuf[i] = sbuf[i];
}

main(argc,argv)
int argc;
char *argv[];

{

int erdf, i, n;

char buff[256];
unsigned int green[256];
unsigned int red[256];
unsigned int blue[256];
/********************** End structure ************************************/


if ((erdf = open(argv[1],0)) < 0) {
	fprintf(stderr,"Error can not open ERDAS file\n");
	exit(0);
	}

/**************** READ HEAD ********************/
n = read(erdf,buff,128);
if (n!=128) {
	fprintf(stderr,"Error in reading header\n");
	exit(0);
	}

/************ READ GREEN values *********************/
n = read(erdf,buff,256);
if (n!=256) {
	fprintf(stderr,"Error in reading header\n");
	exit(0);
	}
convchar(buff,green);
/************ READ RED values *********************/
n = read(erdf,buff,256);
if (n!=256) {
	fprintf(stderr,"Error in reading header\n");
	exit(0);
	}
convchar(buff,red);
/************ READ BLUE values *********************/
n = read(erdf,buff,256);
if (n!=256) {
	fprintf(stderr,"Error in reading header\n");
	exit(0);
	}
convchar(buff,blue);
for (i=0;i<256;i++) {
	fprintf(stderr,"cat %d r_%d g_%d b_%d\n",i,red[i],green[i],blue[i]);
	}
}
