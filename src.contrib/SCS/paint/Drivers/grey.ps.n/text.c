#include <stdio.h>
#include "gis.h"
#define HEADSPACE 210 /* roughly 3 * 72 per inch */
#define LEFTMAR 36 /* .5 inch * 72 */

static char *head_file = NULL;

Palpha()
{
}


Ptext(s) char *s;
{
static int file = 0;
FILE *fileptr;

if (! file) {
	head_file = (char *)G_tempfile();
	file = 1;
	}
if ((fileptr = fopen(head_file,"a")) == NULL) {
	fprintf(stderr,"ERROR can not open temporary file %s\n",head_file);
	exit();
	}
fprintf(fileptr,"%s\n",s);
fclose(fileptr);
}

Pfinish()
{
FILE *fileptr;
int x=0, y=0, r, c, size, incr=0 ;
float lmar;
char buf[128],buf2[135];;

/*  Get temp file and determine amount of text */
Pnpixels(&r,&c);
if (head_file != NULL)
if ((fileptr = fopen(head_file,"r")) == NULL) {
	fprintf(stderr,"ERROR can not open temporary file %s\n",head_file);
	exit;
	}
while (G_getl (buf, sizeof buf, fileptr)){
	++y;
	if (strlen(buf) > x) x = strlen(buf);
	}
fclose(fileptr);

/* Calculate SIZE and MARGIN */
if ( HEADSPACE /y  < c/x) size = HEADSPACE /y;
else size = c/x;
if (size > 12) size = 12;
lmar = (c - ( size * x / 2.0))/2.0;
y = r;

/* reopen file and process */
if ((fileptr = fopen(head_file,"r")) == NULL) {
	fprintf(stderr,"ERROR can not open temporary file %s\n",head_file);
	exit;
	}

/* DO First Line in Bold */
Pouts("\ngrestore\n");
sprintf(buf,"/Helvetica-Bold findfont %d scalefont setfont\n",size);
Pouts(buf);
sprintf(buf,"%.1f %d moveto\n",lmar,y + HEADSPACE - incr);
Pouts(buf);
G_getl (buf, sizeof buf, fileptr);
sprintf(buf2,"(%s) show\n",buf);
Pouts(buf2);
incr += size;

/* Do rest of file */
sprintf(buf,"/Helvetica findfont %d scalefont setfont\n",size);
Pouts(buf);
buf[0] = NULL;
while (G_getl (buf, sizeof buf, fileptr)){
	sprintf(buf2,"%.1f %d moveto\n",lmar,y + HEADSPACE - incr);
	Pouts(buf2);  
	sprintf(buf2,"(%s) show\n",buf);
	Pouts(buf2);
	incr += size;
	}
Pouts("newpath\n");
sprintf(buf2,"%.1f %d moveto\n",lmar ,y + HEADSPACE);
Pouts(buf2);
sprintf(buf2,"%.1f 0 rlineto\n", c * size);
Pouts(buf2);
sprintf(buf2,"0 %d rlineto\n",-(HEADSPACE));
Pouts(buf2);
sprintf(buf2,"%.1f 0 rlineto\n",- (c * size));
Pouts(buf2);
sprintf(buf2,"%.1f %d lineto\n",lmar ,y + HEADSPACE);
Pouts(buf2);
Pouts("closepath stroke\n");

	Pouts( "\n" );
	Pouts( "grestore\n" ); 
	Pouts( "showpage\n" );
	Pouts( "%%Trailer\n" );
}
