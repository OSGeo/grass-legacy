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

dxf_open(filename)
char *filename;
{
	FILE *fopen();

	if((fpdxf = fopen(filename, "w")) == NULL)	{
		fprintf(stderr,
			"ERROR, autocad file:%s cannot be opened\n",
			filename);
		exit(1);
	}
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

dxf_limits(top, bottom, right, left)
double top, bottom, right, left;
{
	fprintf(fpdxf,"  9\n$LIMMIN\n 10\n%lf\n 20\n%lf\n",
		left, bottom);
	fprintf(fpdxf,"  9\n$LIMMAX\n 10\n%lf\n 20\n%lf\n",
		right, top);
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

dxf_solidline()
{
	fprintf(fpdxf,"  0\nLTYPE\n  2\nCONTINUOUS\n 70\n");
	fprintf(fpdxf,"    64\n  3\nSolid line\n 72\n    65\n");
	fprintf(fpdxf," 73\n     0\n 40\n0.0\n");
}

dxf_layer0()
{
	fprintf(fpdxf,"  0\nLAYER\n  2\n0\n 70\n     0\n");
	fprintf(fpdxf," 62\n     7\n  6\nCONTINUOUS\n");
}

dxf_layer(name,color,linetype,frozen)
char *name, *linetype;
int color, frozen;
{
	int is_frozen;

	if(frozen) is_frozen = 1;
	else is_frozen = 64;
	fprintf(fpdxf,"  0\nLAYER\n  2\n%s\n 70\n",name);
	fprintf(fpdxf,"%6d\n 62\n%6d\n  6\n%s\n",
		is_frozen, color, linetype);
}

/* entities
*/

dxf_polyline(layer)
    char *layer;
{
	fprintf(fpdxf,"  0\nPOLYLINE\n  8\n%s\n 66\n",(layer));
	fprintf(fpdxf,"     1\n 10\n0.0\n 20\n0.0\n 30\n0.0\n");
}

dxf_vertex(layer,x,y,z)
char layer[];
double x,y,z;
{
	fprintf(fpdxf,"  0\nVERTEX\n  8\n%s\n 10\n%lf\n",
		layer,x);
	fprintf(fpdxf," 20\n%lf\n 30\n%lf\n", y, z);
}

#define dxf_poly_end(layer) \
	fprintf(fpdxf,"  0\nSEQEND\n  8\n%s\n",(layer))
	
dxf_text(layer,x,y,z,size,just,text)
char layer[], text[];
int just;
double x,y,z,size;
{
	fprintf(fpdxf,"  0\nTEXT\n  8\n%s\n 10\n%lf\n 20\n",layer,x);
	fprintf(fpdxf,"%lf\n 30\n%lf\n 40\n%lf\n  1\n%s\n",y,z,size,text);
	if(just)
	    fprintf(fpdxf," 72\n%6d\n 11\n%lf\n 21\n%lf\n 31\n%lf\n",
			just,x,y,z);
}
