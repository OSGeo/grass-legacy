/******************************************************************************
 * mif.l [v.in.mif]
 * Scanner file for import of MIF/MID data

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 10th. Aug. 2000
 * Last updated 24th. Oct. 2000
 *

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/

%{

/* 
 *
 *  Rules for import of MIF/MID files. Read MIF file then wrap to MID.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "gis.h"
#include "Vect.h"
#include "local_structs.h"
#include "scanner.h"

#ifdef yywrap
#undef yywrap
#endif

extern struct line_pnts *lrep; /* Define in main() function */
extern site_array *site0;
extern line_array *line0;
extern line_array *area0;
extern type_array *type0;
extern int header_only;
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
extern int nglines;
extern int nrings;
extern int site_index;
extern int line_index;
extern int region_index;
extern int line_grps;
extern int ring_grps;
extern int recs;
extern int has_mid;
extern FILE *mif_file;
extern FILE *mid_file;

struct line_pnts *linex;
int allocreg = 0;
int nlines;
int ncols;
int slot, pos;
int data_is_set = 0;
int cnt;
int nparts, numparts, nvertices;

char *ssx, *ssy, *ssx1, *ssy1, *ssx2, *ssy2;
double sx, sy, sx1, sy1, sx2, sy2;

char dels[4];
d_type del0;
char delchar;

int current_region, current_ring = 0;

struct line_pnts *tmp_line;

%}

whitespace [ \t]+
float (\+|\-)?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))
version (Version|VERSION|version)
charset (Charset|CHARSET|charset)
delimiter (Delimiter|DELIMITER|delimiter)
coordsys (Coordsys|CoordSys|COORDSYS|coordsys)
midcols (Columns|COLUMNS|columns)
datastart (Data|DATA|data)
blank ^\n
end (\n|\r|\r\n)
point (Point|POINT|point)
line (LINE|line|Line)
pline (PLINE|pline|Pline)
mult (MULTIPLE|multiple|Multiple)
ldata [ \t]*{float}[ \t]+{float}[ \t]*
region (REGION|region|Region)
pen (Pen|pen|PEN){whitespace}
brush (Brush|BRUSH|brush)
symbol (SYMBOL|Symbol|symbol)
center (Center|CENTER|center)
param \(.*\)

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
%x LDATALINES
%x MLDATALINES
%x RDATALINES
%x GPOINT
%x GLINE
%x GPLINE
%x GMLINE
%x REGION
%x NUMLINES
%x DATALINES
%x PEN
%x BRUSH
%x SYMBOL
%x CENTER
%x MID
%x MIDLINE

%%

{version}       	        {BEGIN MIF_VERSION;}

<MIF_VERSION>[0-9]+		{strcpy(version_no, yytext);}

<MIF_VERSION>{end}		{BEGIN 0;}

{charset}       	        {BEGIN CHARSET;}

<CHARSET>[A-Za-z_0-9\"]+	{strcpy(chset, yytext);}

<CHARSET>{end}          {BEGIN 0;}

{delimiter}	        {BEGIN DELIM;}

<DELIM>\".\"	        {
                          strcpy(del0, yytext);
                          delchar = del0[1]; 
			  dels[0] = del0[1];
			  dels[1] = '(';
			  dels[2] = ')';
			  dels[3] = '\0';
			}

<DELIM>{end}            {BEGIN 0;}

{coordsys}      	{BEGIN COORDSYS;}

<COORDSYS>.*	        {strcpy(proj_info, yytext);}

<COORDSYS>{end}         {BEGIN 0;}

{midcols}	        {BEGIN COLS;}

<COLS>[0-9]+		{
                          numcols = atoi(yytext);
			  ncols = 0;
			}

<COLS>[A-Za-z_0-9]+{whitespace}	        {
                                          strcpy(&field_info[ncols][0], yytext);
                                          BEGIN COLTYPE;
                                        }
<COLTYPE>{typef}        {
                          strcpy(&field_type[ncols++][0], yytext);
			  BEGIN COLS;
                        }

<COLS>{datastart}	{
                          if(ncols != numcols)  {
			    fprintf(stderr, "WARNING: Number of fields does\
					not match specified number");
			    numcols = ncols;
			  }
			  nsites = 0; nglines = 0; nrings = 0; ncenter = 0;
			  site_index = line_index = region_index = 0;
			  type0 = type_array_init();
			  if(header_only) return 1;
			  BEGIN DATA;
			}

<DATA>{point}           {
                          if( append_type_spec(type0, DOT, site_index++ ) < 0 ) {
			    return -1;
			  }
			  BEGIN GPOINT;
                        }

<GPOINT>{float}{whitespace}{float}[ \t]*       { 
                                                 ssx = strtok(yytext, " \t");
                                                 sx = atof(ssx);
						 ssy = strtok(NULL, " \t");
						 sy = atof(ssy);
						 if(alloc_site_space( site0, nsites + 1))
						   return -1;
						 site0->x[nsites] = sx;
						 site0->y[nsites] = sy;
						 nsites++;
					       }

<GPOINT>{end}                                  {  BEGIN DATA; }

<DATA>[ \t]*Symbol[ \t]*.*                     ;

<DATA>{line}                                   { 
                                                 if( append_type_spec(type0, LINE, line_index++ ) < 0 ) {
						   return -1;
						 }

						 BEGIN GLINE; 
                                               }
                            
<GLINE>({float}{whitespace}){3}{float}                { 
                                                        ssx1 = strtok(yytext, " \t");
                                                        sx1 = atof(ssx1);
							ssy1 = strtok(NULL, " \t");
							sy1 = atof(ssy1);
							ssx2 = strtok(NULL, " \t");
							sx2 = atof(ssx2);
							ssy2 = strtok(NULL, " \t");
							sy2 = atof(ssy2);
							if(alloc_glines( line0, nglines + 1))
							  return -1;
							append_simple_line(line0, sx1, sy1, sx2, sy2,
									   nglines++);
                                                       }
<GLINE>{end}             { BEGIN DATA;}

<DATA>{pline}            { 
			   BEGIN GPLINE;
                         }

<GPLINE>{mult}           {
			   BEGIN GMLINE;
			   numparts = 0;
                         }

<GPLINE>[1-9][0-9]*      { 
                           if( append_type_spec(type0, LINE, line_index++ ) < 0 ) {
			     return -1;
			   }
                           linex = Vect_new_line_struct();                
                           nvertices = atoi(yytext);
                           BEGIN LDATALINES; 
                         }

<LDATALINES>{ldata}                 { 
                                      ssx = strtok(yytext, " \t");
                                      sx = atof(ssx);
				      ssy = strtok(NULL, " \t");
				      sy = atof(ssy);
				      Vect_append_point(linex, sx, sy);
				      if(linex->n_points == nvertices) {
					if(alloc_glines( line0, nglines + 1))
					  return -1;
					memcpy(&line0->gplines[nglines++], linex, sizeof(struct line_pnts) );
					free(linex);
					BEGIN DATA;
				      }
                                    }

<GMLINE>[1-9][0-9]*       { 
                            nparts = atoi(yytext);
			    BEGIN MLDATALINES;
                          }

<MLDATALINES>[1-9][0-9]*             {
                                       if( append_type_spec(type0, LINE, line_index ) < 0 ) {
					 return -1;
				       }
                                       numparts++;
				       linex = Vect_new_line_struct();                
				       nvertices = atoi(yytext);
                                     }


<MLDATALINES>{ldata}/{end}           {
                                       ssx = strtok(yytext, " \t");
				       sx = atof(ssx);
				       ssy = strtok(NULL, " \t");
				       sy = atof(ssy);
				       Vect_append_point(linex, sx, sy);
				       if(linex->n_points == nvertices) {
					 if(alloc_glines( line0, nglines + 1))
					   return -1;
					 memcpy(&line0->gplines[nglines++], linex, sizeof(struct line_pnts) );
					 free(linex);
					 if(numparts == nparts) {
					   line_index++;
					   BEGIN DATA;
					 }
				       }
                                     }


<DATA>{region}           { 
			   numparts = 0;
			   BEGIN REGION;
                         }


<REGION>[1-9][0-9]*       { 
                            nparts = atoi(yytext);
			    BEGIN RDATALINES;
                          }

<RDATALINES>[1-9][0-9]*             {
                                       if( append_type_spec(type0, AREA, region_index ) < 0 ) {
					 return -1;
				       }
                                       numparts++;
				       linex = Vect_new_line_struct();                
				       nvertices = atoi(yytext);
                                     }


<RDATALINES>{ldata}/{end}           {
                                       ssx = strtok(yytext, " \t");
				       sx = atof(ssx);
				       ssy = strtok(NULL, " \t");
				       sy = atof(ssy);
				       Vect_append_point(linex, sx, sy);
				       if(linex->n_points == nvertices) {
					 if(alloc_glines( area0, nrings + 1))
					   return -1;
					 memcpy(&area0->gplines[nrings++], linex, sizeof(struct line_pnts) );
					 free(linex);
					 if(numparts == nparts) {
					   region_index++;
					   BEGIN DATA;
					 }
				       }
                                     }

<DATA>{center}          {
                          BEGIN CENTER;
			  cnt = 0;
                        }
			
<CENTER>{float}                 	 {

                                           int j; /* loop variable */

                                           if(ncenter == 0) {
					     if( (xcenter = (double *)G_malloc(20 *
									       sizeof(double)) ) == NULL )
					       return -1;
					     if( (ycenter = (double *)G_malloc(20 *
									       sizeof(double)) ) == NULL )
					       return -1;

					     ncenter = 20;

					     for( j = 0; j < 20; j++ ) {
					       xcenter[j] = -999999999.999;
					       ycenter[j] = -999999999.999;
					     }

					   }
					     
					   else if(nrings + 1 > ncenter) {
					     if( (xcenter = (double *)realloc(xcenter, (ncenter + 20) *
									      sizeof(double)) ) == NULL )
					       return -1;
					     if( (ycenter = (double *)realloc(ycenter, (ncenter + 20) *
									      sizeof(double)) ) == NULL )
					       return -1;

					     ncenter += 20;

					     for( j = ncenter - 20; j < ncenter; j++ ) {
					       xcenter[j] = -999999999.999;
					       ycenter[j] = -999999999.999;
					     }

					   }
					   
					   if(cnt++) ycenter[region_index - 1] = atof(yytext);
					   else xcenter[region_index - 1] = atof(yytext);
					   
					   if(cnt > 1) BEGIN DATA;
                                         }

<DATA>{pen}              {
                           BEGIN PEN;
                         }

<DATA>{brush}            {
                           BEGIN BRUSH;
                         }

<DATA>{symbol}           { 
                           BEGIN SYMBOL;
                         }

<PEN>{param}             {
                           parse_pen(yytext);
			   BEGIN DATA;
                         }

<BRUSH>{param}           {
                           parse_brush(yytext);
			   BEGIN DATA;
                         }

<SYMBOL>{param}          {
                           BEGIN DATA;
                         }

			
<MID>[^ \t].*[ \t]*	{ 
                          BEGIN MIDLINE;
			  if(!data_is_set) {
			    data_is_set = 1;
			    data_info = (d_type *)G_malloc( (nsites + nglines) *
							    sizeof(d_type));
			  }
			  strcpy(&data_info[recs][0], yytext);
			}

<MIDLINE>{end}		{
                          BEGIN MID;
			  recs++;
			}
		
<*>{whitespace} |
	{blank} |
	  {end}		;


%%

	
void parse_pen(char *pen_info)  {
	int fx1, fx2; 
	long fx3;
	char *ff1, *ff2, *ff3;
	
	ff1 = strtok(pen_info, ",() ");
	fx1 = atoi(ff1);

	ff2 = strtok(NULL, ",() ");
	fx2 = atoi(ff2);

	ff3 = strtok(NULL, ",() ");
	fx3 = atol(ff3);
	
	proc_pen_info(SET_VAL, &fx1, &fx2, &fx3);
}

int proc_pen_info(int fflag, int *f1, int *f2, long *f3) {
	static int fs1 = 1;
	static int fs2 = 2;
	static long fs3 = 0;
	
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
	int brx1; 
	long brx2, brx3;
	char *b1, *b2, *b3;
	
	b1 = strtok(brush_info, ",() ");
	brx1 = atoi(b1);

	b2 = strtok(NULL, ",() ");
	brx2 = atol(b2);

	if( ( b3 = strtok(NULL, ",() ")) == NULL )
	  brx3 = 16777215;
	else 
	  brx3 = atol(b3);
	
	proc_brush_info(SET_VAL, &brx1, &brx2, &brx3);
	
}

int proc_brush_info(int fflag, int *br1, long *br2, long *br3) {
	static int brs1 = 1;
	static long brs2 = 0;
	static long brs3 = 16777215;
	
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

int alloc_glines ( line_array *l1, int size )  {

    if( size >= l1->alloc_glines ) {
      if( (l1->gplines = (struct line_pnts *)realloc( l1->gplines, (l1->alloc_glines + 128) * 
						      sizeof(struct line_pnts))) == NULL ) {
					fprintf(stderr, "Unable to allocate space\n");
					return -1;
      }


      l1->alloc_glines += 128;
      
    }

    return 0;
}


int append_simple_line(line_array *l0, const double x1, const double y1, const double x2,
		       const double y2, const int nlines) {

  /* local */

  double xm, ym;
  struct line_pnts *tmpline;

  if(0 > alloc_glines( l0, nlines + 1))
    return -1;

  xm = ( x1 + x2 ) / 2.0;
  ym = ( y1 + y2 ) / 2.0;

  tmpline = &l0->gplines[nlines]; /* Find the current line */

  if( (tmpline->x = (double *)malloc( 3 * sizeof(double) )) == NULL ) {
    fprintf(stderr, "Unable to allocate space for line\n");
    return -1;
  }

  if( (tmpline->y = (double *)malloc( 3 * sizeof(double) )) == NULL ) {
    fprintf(stderr, "Unable to allocate space for line\n");
    return -1;
  }
  
  tmpline->n_points = tmpline->alloc_points = 3;
  tmpline->x[0] = x1; tmpline->y[0] = y1;
  tmpline->x[1] = xm; tmpline->y[1] = ym;
  tmpline->x[2] = x2; tmpline->y[2] = y2;

  return 0;
}


type_array *type_array_init(void) {

  /* Initialise type array */

  type_array *tp0;

  tp0 = (type_array *)G_malloc( sizeof(type_array) );
  tp0->list = (char *)G_malloc( 128 );
  tp0->entities = (int *)G_malloc( 128 * sizeof(int) );
  tp0->alloc_chars = 128;
  tp0->n_chars = 0;

  return tp0;
}

int append_type_spec(type_array *arr0, const char val, const int cnt) {

  if(arr0->n_chars == 0) {
    if( (arr0->list = (char *)malloc(128) ) == NULL ) {
      fprintf(stderr, "Unable to allocate space\n");
      return -1;
    }

    else {
      arr0->alloc_chars = 128;
    }
    
  }

  if(arr0->n_chars + 1 > arr0->alloc_chars) {
    if( (arr0->list = (char *)realloc(arr0->list, (arr0->alloc_chars + 128) )) == NULL ) {
      fprintf(stderr, "Unable to allocate space\n");
      return -1;
    }

    if( (arr0->entities = (int *)realloc(arr0->entities, 
					  (arr0->alloc_chars + 128) * sizeof(int) )) == NULL ) {
      fprintf(stderr, "Unable to allocate space\n");
      return -1;
    }

    else {
      arr0->alloc_chars += 128;
    }
    
  }

  arr0->list[arr0->n_chars] = val;
  arr0->entities[arr0->n_chars] = cnt;
  arr0->n_chars++;

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
