/* to.dxf.h is a header file to facilitate the transfer of  
** information to the dxf format.  It attempts to be input
** nuetral (so when digit gets replaced ten years from now
** it will still be good...).
**
** This header file supports Version 10 of dxf 
**
** written by: Chuck Ehlschlaeger
**
** last revised: March 10, 1989
*/

#include <stdio.h>
#include <ctype.h>

FILE	*fpdxf;

int dxf_open (char *filename)
{
	if((fpdxf = fopen(filename, "w")) == NULL)	{
		fprintf(stderr,
			"ERROR, autocad file:%s cannot be opened\n",
			filename);
		exit(1);
	}

	return 0;
}

#define dxf_header() fprintf(fpdxf,"  0\nSECTION\n  2\nHEADER\n")
#define dxf_tables() fprintf(fpdxf,"  0\nSECTION\n  2\nTABLES\n")
#define dxf_blocks() fprintf(fpdxf,"  0\nSECTION\n  2\nBLOCKS\n")
#define dxf_entities() \
	fprintf(fpdxf,"  0\nSECTION\n  2\nENTITIES\n")
#define dxf_endsec() fprintf(fpdxf,"  0\nENDSEC\n")
#define dxf_eof() fprintf(fpdxf,"  0\nEOF\n"); \
		fclose(fpdxf)

/* header stuff	
*/

int dxf_limits (double top, double bottom, double right, double left)
{
	fprintf(fpdxf,"  9\n$LIMMIN\n 10\n%f\n 20\n%f\n",
		left, bottom);
	fprintf(fpdxf,"  9\n$LIMMAX\n 10\n%f\n 20\n%f\n",
		right, top);

	return 0;
}

/* tables stuff
*/

#define dxf_linetype_table(numlines) \
	fprintf(fpdxf,"  0\nTABLE\n  2\nLTYPE\n 70\n%6d\n", \
		(numlines))
#define dxf_layer_table(numlayers) \
	fprintf(fpdxf,"  0\nTABLE\n  2\nLAYER\n 70\n%6d\n", \
		(numlayers))
#define dxf_endtable() fprintf(fpdxf,"  0\nENDTAB\n")

int dxf_solidline (void)
{
	fprintf(fpdxf,"  0\nLTYPE\n  2\nCONTINUOUS\n 70\n");
	fprintf(fpdxf,"    64\n  3\nSolid line\n 72\n    65\n");
	fprintf(fpdxf," 73\n     0\n 40\n0.0\n");

	return 0;
}

int dxf_layer0 (void)
{
	fprintf(fpdxf,"  0\nLAYER\n  2\n0\n 70\n     0\n");
	fprintf(fpdxf," 62\n     7\n  6\nCONTINUOUS\n");

	return 0;
}

int dxf_layer (char *name, int color, char *linetype, int frozen)
{
	int is_frozen;

	if(frozen) is_frozen = 1;
	else is_frozen = 64;
	fprintf(fpdxf,"  0\nLAYER\n  2\n%s\n 70\n",name);
	fprintf(fpdxf,"%6d\n 62\n%6d\n  6\n%s\n",
		is_frozen, color, linetype);

	return 0;
}

/* entities
*/

int dxf_point (char *layer, double x, double y, double z)
{
	fprintf(fpdxf,"0\nPOINT\n");
	fprintf(fpdxf,"8\n%s\n",(layer));
	fprintf(fpdxf,"10\n%f\n20\n%f\n30\n%f\n", x, y, z);

	return 0;
}

int dxf_polyline (char *layer)
{
	fprintf(fpdxf,"0\nPOLYLINE\n");
	fprintf(fpdxf,"8\n%s\n",(layer));
	fprintf(fpdxf,"66\n1\n");
	/* fprintf(fpdxf,"10\n0.0\n 20\n0.0\n 30\n0.0\n"); */ /* ? */

	return 0;
}

int dxf_vertex (char layer[], double x, double y, double z)
{
	fprintf(fpdxf,"0\nVERTEX\n");
	fprintf(fpdxf,"8\n%s\n", layer);
	fprintf(fpdxf,"10\n%f\n20\n%f\n 30\n%f\n", x, y, z);

	return 0;
}

#define dxf_poly_end(layer) \
	fprintf(fpdxf,"  0\nSEQEND\n  8\n%s\n",(layer))
	
int dxf_text (char layer[], double x, double y, double z, double size, int just, char text[])
{
	fprintf(fpdxf,"  0\nTEXT\n  8\n%s\n 10\n%f\n 20\n",layer,x);
	fprintf(fpdxf,"%f\n 30\n%f\n 40\n%f\n  1\n%s\n",y,z,size,text);
	if(just)
	    fprintf(fpdxf," 72\n%6d\n 11\n%f\n 21\n%f\n 31\n%f\n",
			just,x,y,z);

	return 0;
}

