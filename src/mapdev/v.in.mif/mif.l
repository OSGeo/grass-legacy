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

char dels[3];
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
end \n
point (Point|POINT|point)
line (LINE|line|Line)
pline (PLINE|pline|Pline)
mult (MULTIPLE|multiple|Multiple)
ldata [ \t]*{float}[ \t]+{float}[ \t]*
region (REGION|region|Region){whitespace}
word [a-z0-9_A-Z]*{whitespace}
chardef Char\([0-9]+\)
pen (Pen|pen|PEN){whitespace}\(.*\)
brush (Brush|BRUSH|brush){whitespace}\(.*\)
symbol (SYMBOL|Symbol|symbol){whitespace}\(.*\)
center Center{whitespace}

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
%x GPOINT
%x GLINE
%x GPLINE
%x GMLINE
%x REGION
%x NUMLINES
%x DATALINES
%x PEN
%x BRUSH
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
			  dels[2] = '\0';
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
			  nsites = 0; nglines = 0;
			  type0 = type_array_init();
			  if(header_only) return 1;
			  BEGIN DATA;
			}

<DATA>{point}           {
                          if( append_type_spec(type0, DOT) < 0 ) {
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
                                                 if( append_type_spec(type0, LINE) < 0 ) {
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
                           if( append_type_spec(type0, LINE) < 0 ) {
			     return -1;
			   }
			   BEGIN GPLINE;
                         }

<GPLINE>{mult}           {
                           if( append_type_spec(type0, LINE) < 0 ) {
			     return -1;
			   }
			   BEGIN GMLINE;
			   numparts = 0;
                         }

<GPLINE>[1-9][0-9]*      { 
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
					 if(numparts == nparts)
					   BEGIN DATA;
				       }
                                     }


<DATA>{region}     {   
                       BEGIN REGION;
		       current_ring++;
		       if( allocreg == 0 )  {
			 if( (lrep = (struct line_pnts *)malloc( 50 *
								 sizeof(struct line_pnts) )) == NULL )
			   return -1;	
			 allocreg = 50;
		       }
		       else if(current_ring > allocreg) {
			 if( (lrep = (struct line_pnts *)realloc(lrep, (allocreg + 20) *
								 sizeof(struct line_pnts) )) == NULL )
			   return -1;
			 allocreg += 20;
		       }
		       tmp_line = Vect_new_line_struct();
		       memcpy(&lrep[current_ring-1], tmp_line, sizeof(struct line_pnts));
		       Vect_destroy_line_struct(tmp_line);
		   }

<REGION>[0-9]+	{BEGIN NUMLINES; current_region = atoi(yytext);}


<NUMLINES>[0-9]+  {BEGIN DATALINES; nlines = atoi(yytext);
			cnt = 0;}


			

<CENTER>{float}		{
                          if(current_region + 1 > ncenter) {
			    if( (xcenter = (double *)realloc(xcenter, (ncenter + 20) *
							     sizeof(double)) ) == NULL )
			      return -1;
			    if( (ycenter = (double *)realloc(ycenter, (ncenter + 20) *
							     sizeof(double)) ) == NULL )
			      return -1;
			    ncenter += 20;
			  }
			  
			  if(cnt++) xcenter[current_ring-1] = atof(yytext);
			  else ycenter[current_ring-1] = atof(yytext);
			  
			  if(cnt > 1) BEGIN MID;
			}

<DATA>{pen} |
    {brush} |
   {symbol}             ;


			
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
  tp0->alloc_chars = 128;
  tp0->n_chars = 0;

  return tp0;
}

int append_type_spec(type_array *arr0, const char val) {

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

    else {
      arr0->alloc_chars += 128;
    }
    
  }

  arr0->list[arr0->n_chars] = val;
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
