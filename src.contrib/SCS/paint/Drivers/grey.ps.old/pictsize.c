#include <stdio.h>
#include <math.h>
#define HEADSPACE 210 /* roughly 3 * 72 per inch */
#define FACTOR 1

Ppictsize (rows, cols)
{
    char buf[128];
	float scols, srows, llx, lly, scale=1.0;
	int bps=8, x, y, i, bufsize;

	Pnpixels(&y,&x);
	scols = cols * FACTOR;
	srows = rows * FACTOR;
	llx = ( x - scols ) / 2.0;
	lly = ( y - srows ) / 2.0;
	bufsize = 3 * cols;
	for (i=1;i<4;i++)  
		if (fmod((double)cols,(double)i) == 0) bufsize = 3 * cols/i;
fprintf(stderr,"rows_%d cols_%d y_%d x_%d b_%d\n",rows,cols,y,x,bufsize	);


	sprintf(buf,
	"%%%%BoundingBox: %d %d %d %d\n",
		0, 0, x, y+HEADSPACE);
	Pouts(buf);
	sprintf( buf,"%%%%EndComments\n" );
	Pouts(buf);
	sprintf( buf,"%%%%EndProlog\n" );
	Pouts(buf);
	sprintf( buf,"%%%%Page: 1 1\n" );
	Pouts(buf);
	sprintf( buf,"/picstr %d string def\n", bufsize );
	Pouts(buf);
	sprintf( buf,"gsave\n" );
	Pouts(buf);
	sprintf( buf,"%g %g translate\n", llx, lly );
	Pouts(buf);
	sprintf( buf,"%g %g scale\n", scols, srows );
	Pouts(buf);
	sprintf( buf,"%d %d %d\n", cols, rows, bps );
	Pouts(buf);
	sprintf( buf,"[ %d 0 0 -%d 0 %d ]\n", cols, rows, rows );
	Pouts(buf);
	sprintf( buf,"{ currentfile picstr readhexstring pop }\n" );
	Pouts(buf);
	sprintf( buf,"false 3\n" );
	Pouts(buf);
	sprintf( buf,"colorimage\n" );
	Pouts(buf);
}
