%{

/* 
 *
 *  Rules for import of MIF/MID files. Read MIF file then wrap to MID.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "local_structs.h"
#include "scanner.h"

#ifdef yywrap
#undef yywrap
#endif

extern struct line_pnts *lrep; /* Define in main() function */
extern site_array *site0;
extern double *xcenter;
extern double *ycenter;
extern int ncenter;
extern d_type version_no;
extern d_type chset;
extern d_type proj_info;
extern d_type *data_info;
extern field_data field_info;
extern field_data field_type;
extern int numcols;
extern int nsites;
extern int recs;
extern int has_mid;
extern FILE *mif_file;
extern FILE *mid_file;

int allocreg = 0;
int nlines;
int ncols;
int slot, pos;
int data_is_set = 0;
int cnt;

char *ssx, *ssy;
double sx, sy;

char dels[3];
d_type del0;
char delchar;

int current_region, current_ring = 0;

struct line_pnts *tmp_line;

%}

version ^Version[ \t]+
charset ^Charset[ \t]+
delimiter ^Delimiter[ \t]+
coordsys ^CoordSys[ \t]+
midcols ^Columns[ \t]+
datastart ^Data
blank ^\n
end \n
point ^Point[ \t]+
region ^Region[ \t]+
word [a-z0-9_A-Z]*[ \t]+
whitespace [ \t]+
chardef Char\([0-9]+\)
float (\+|\-)?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))
pen Pen[ \t]+
brush Brush[ \t]+
center Center[ \t]+

charf [Cc]har[ \t]*\([ \t]*[1-9][0-9]*[ \t]*\)
intf [Ii]nteger
sintf [Ss]mallint
decf [Dd]ecimal[ \t]*\([ \t]*[1-9][0-9]*[ \t]*,[ \t]*[1-9][0-9]*[ \t]*\)
floatf [Ff]loat
datef [Dd]ate
logf [Ll]ogical
typef {charf}|{intf}|{sintf}|{decf}|{floatf}|{datef}|{logf}

%x MIF_VERSION
%x CHARSET
%x DELIM
%x COORDSYS
%x COLS
%x COLTYPE
%x DATA
%x GPOINT
%x REGION
%x NUMLINES
%x DATALINES
%x PEN
%x BRUSH
%x CENTER
%x MID
%x MIDLINE

%%

{version}       	{BEGIN MIF_VERSION;}

<MIF_VERSION>[0-9]+		{strcpy(version_no, yytext);}
<MIF_VERSION>{end}		{BEGIN 0;}

{charset}       	{BEGIN CHARSET;}

<CHARSET>[A-Za-z_0-9\"]+	{strcpy(chset, yytext);}
<CHARSET>{end}          {BEGIN 0;}

{delimiter}	        {BEGIN DELIM;}

<DELIM>\".\"	        {strcpy(del0, yytext);
                          delchar = del0[1]; 
			  dels[0] = del0[1];
			  dels[1] = '(';
			  dels[2] = '\0';
			}
<DELIM>{end}            {BEGIN 0;}

{coordsys}      	{BEGIN COORDSYS;}

<COORDSYS>.*	        {strcpy(proj_info, yytext);}
<COORDSYS>{end}         {BEGIN 0;}

{midcols}	        {BEGIN COLS;}

<COLS>[0-9]+		{numcols = atoi(yytext);
			  ncols = 0;
			}

<COLS>[A-Za-z_0-9]+[ \t]+	{strcpy(&field_info[ncols][0], yytext);
                                          BEGIN COLTYPE;
                                        }
<COLTYPE>{typef}        {
                          strcpy(&field_type[ncols++][0], yytext);
			  BEGIN COLS;
                        }

<COLS>{datastart}	{if(ncols != numcols)  {
				fprintf(stderr, "WARNING: Number of fields does\
					not match specified number");
				numcols = ncols;
			  }
			 BEGIN DATA;
			}

<DATA>{point}           {BEGIN GPOINT;}

<GPOINT>{float}[ \t]+{float}[ \t]*      { ssx = strtok(yytext, " \t");
                                          sx = atof(ssx);
					  ssy = strtok(NULL, " ");
					  sy = atof(ssy);
					  if(alloc_site_space( site0, nsites + 1))
					     return -1;
					  site0->x[nsites] = sx;
					  site0->y[nsites] = sy;
					  nsites++;
					  BEGIN DATA;
					}

<DATA>[^ \t\n]+                         {;
                                         /* bin everything else */
                                        }

			
<MID>[^ \t].*[ \t]*	{ BEGIN MIDLINE;
			  if(!data_is_set) {
			  	data_is_set = 1;
			  	data_info = (d_type *)G_malloc(nsites *
							       sizeof(d_type));
			  }
			  strcpy(&data_info[recs][0], yytext);
			}

<MIDLINE>{end}		{BEGIN MID;
			  recs++;
			}

		
<*>{whitespace} |
	{blank} |
	{end}		;
%%

	
void parse_pen(char *pen_info)  {
	int fx1, fx2, fx3;
	
	fx1 = atoi(strtok(pen_info, dels));
	fx2 = atoi(strtok(NULL, dels));
	fx3 = atoi(strtok(NULL, dels));
	
	proc_pen_info(SET_VAL, &fx1, &fx2, &fx3);
}

int proc_pen_info(int fflag, int *f1, int *f2, int *f3) {
	static int fs1 = 1;
	static int fs2 = 2;
	static int fs3 = 0;
	
	if(fflag == SET_VAL)  {
		fs1 = *f1;
		fs2 = *f2;
		fs3 = *f3;
		return 0;
	}
	else if(fflag == GET_VAL)  {
		*f1 = fs1;
		*f2 = fs2;
		*f3 = fs3;
		return 0;
	}
	else return -1;
	
}

void parse_brush(char *brush_info)  {
	int brx1, brx2, brx3;
	
	brx1 = atoi(strtok(brush_info, dels));
	brx2 = atoi(strtok(NULL, dels));
	brx3 = atoi(strtok(NULL, dels));
	
	proc_brush_info(SET_VAL, &brx1, &brx2, &brx3);
	
}

int proc_brush_info(int fflag, int *br1, int *br2, int *br3) {
	static int brs1 = 1;
	static int brs2 = 0;
	static int brs3 = 16777215;
	
	if(fflag == SET_VAL)  {
		brs1 = *br1;
		brs2 = *br2;
		brs3 = *br3;
		return 0;
	}
	else if(fflag == GET_VAL)  {
		*br1 = brs1;
		*br2 = brs2;
		*br3 = brs3;
		return 0;
	}
	else return -1;
	
}

int alloc_site_space ( site_array *s1, int size )  {

    if( size >= s1->alloc_sites ) {
      if( (s1->x = (double *)realloc( s1->x, (s1->alloc_sites + 128) * sizeof(double))) == NULL ) {
					fprintf(stderr, "Unable to allocate space\n");
					return -1;
      }

      if( (s1->y = (double *)realloc( s1->y, (s1->alloc_sites + 128) * sizeof(double))) == NULL ) {
					fprintf(stderr, "Unable to allocate space\n");
					return -1;
      }

      s1->alloc_sites += 128;
      
    }

    return 0;
}


int yywrap ()  {

        if(!has_mid) return 1;

	if( yyin == mif_file) {
	        BEGIN MID;
		yyin = mid_file;
		return 0;
	}
	else return 1;
}
