/*  @(#)snooper.c     1.0  10/03/89   
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector maps, areas, lines, islands, etc.
 */

#include  "gis.h"
#include "digit.h"
#include "dig_head.h"

#define		DIG_DIR		"dig"

struct Map_info Map;
char  buf[1024] ;
struct head Head;
struct line_pnts Points;

main (argc,argv)
int argc;
char *argv[];

{
	int Full, Area, Line, Isle, Attr;
	int cnt, cat, line, linea, isle, island, larea, rarea;
	register int area_num, i, ii, jj, kk;
	char  input[128], *mapset ;
	double *X, *Y;
	int n_points;


	Full = Area = Line = Isle = Attr = 0;

                                    /*  check for arg. should be some */
        if (argc > 1 )
           {          
/* which arguement */
           for (cnt=1; cnt < argc; cnt++)
             {

/* -------------   -F  All data    -------------------- */    
             if (strncmp (argv[cnt],"-F",2) == 0 ) Full = 1;

/* -------------   -A  area data    -------------------- */    
             else if (strncmp (argv[cnt],"-A",2) == 0 ) Area = 1;

/* -------------   -L  line data    -------------------- */    
             else if (strncmp (argv[cnt],"-L",2) == 0 ) Line = 1;

/* -------------   -I  island data    ------------------ */    
             else if (strncmp (argv[cnt],"-I",2) == 0 ) Isle = 1;

/* -------------   -C  category/attribute data    ------ */    
             else if (strncmp (argv[cnt],"-C",2) == 0 ) Attr = 1;

/* -------------   input file name --------------------- */    
             else 
	        {                          
	        if (sscanf (argv[cnt], "%s",input) != 1) 
	           {
	           fprintf (stderr,"\n Error in input file name ");
	           exit(1);
	           }
                }
             }    /* end arg. check */

/* ---------------- Executable code --------------------------- */
 
	     if (!Area && !Line && !Isle && !Attr) Full = 1;
             fprintf(stderr,"\nLoading vector information.\n");
                     /* Do initial read of input DIGIT file */
	     if (dig_P_init(input, G_mapset (), &Map ) == -1 )
                {
		G_fatal_error("Reading input file.") ;
		return(-1) ;
                }

                     /* Read and write header info */
	     dig_read_head_binary (Map.digit, &Head);

             fprintf(stderr,"\nProcessing ..... \n");
                     /* Cycle through all areas */
	     for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
	       { 
	            /* get the category for area "area_num" */
               cat = Map.Att[Map.Area[area_num].att].cat;

	       if (Full || Area)
                   printf("\narea= %d, cat= %ld, n_lines = %ld, n_isles= %ld, active= %ld\n",area_num,cat,Map.Area[area_num].n_lines,Map.Area[area_num].n_isles,Map.Area[area_num].alive);
               if (Area)
		  {
                  if (Map.Area[area_num].n_lines)
		     {
                     printf("   lines: ");
                     for (ii=0; ii < Map.Area[area_num].n_lines; ii++)
		        {
                        line = Map.Area[area_num].lines[ii];
                        printf("  %ld ",line);
		        }
                     printf("\n");
		     }
                  if (Map.Area[area_num].n_isles) 
                     {
		     printf("   islands: ");
		     for (ii=0; ii < Map.Area[area_num].n_isles; ii++)
		        { 
		        island= Map.Area[area_num].isles[ii];
			printf("  %ld ",island);
			}
                      printf("\n");
		      }
		  }

/* --------------------- Area Lines Section --------------------------- */
		    /* does "area_num" have any lines ? */
                if ((Full || Line) && Map.Area[area_num].n_lines)
		    /* for every line in the area_lines list */
                      for (ii=0; ii < Map.Area[area_num].n_lines; ii++)
		        {
		               /* set line to absolute value pointed at */
                        linea = abs (Map.Area[area_num].lines[ii]);
                        line = Map.Area[area_num].lines[ii];

                        printf("\n  line= %ld, area_left= %ld, area_right= %ld\n        bounds: N %12.2lf,     S %12.2lf\n                E %12.2lf,     W %12.2lf\n",line,Map.Line[linea].left,Map.Line[linea].right,Map.Line[linea].N,Map.Line[linea].S,Map.Line[linea].E,Map.Line[linea].W);

    	                if (Line)
			   {
    	                   if (0 > dig_Read_line (Map.digit, Map.Line[linea].offset, &X, &Y, &n_points))
			         fprintf (stderr, "Out of Memory\n"), exit (-1);
	                   printf("\n        coordinates:        %6d\n",n_points);
	                   while (n_points--)
	                      printf("                  %12.2lf        %12.2lf\n", *(X++),*(Y++));
			   }
		        }    /* end area lines */

/* --------------------- Area Islands Section ------------------------- */
	                                      /* works for 3.1 or above */
                 if ((Full || Isle) && Map.Area[area_num].n_isles) 
                   for (ii=0; ii < Map.Area[area_num].n_isles; ii++)
		     { 
		          /* set island to element in area isles list */
		     island= Map.Area[area_num].isles[ii];
			    /* get left and right area for this line */
                     larea = Map.Line[linea].left;
                     rarea = Map.Line[linea].right;


             /* Cycle through lines making this island 
		      these are handled like area boundaries */
                     for (kk=0; kk < Map.Isle[island].n_lines; kk++)
		       {
                       linea = abs (Map.Isle[island].lines[kk]);
                       line = Map.Isle[island].lines[kk];
                       printf("\n\t  line= %ld, area_left= %ld, area_right= %ld\n\t        bounds: N %12.2lf,     S %12.2lf\n\t                 E %12.2lf,     W %12.2lf\n",line,Map.Line[linea].left,Map.Line[linea].right,Map.Line[linea].N,Map.Line[linea].S,Map.Line[linea].E,Map.Line[linea].W);

			    /* select the positive side */
		       if (larea > 0) jj = Map.Area[larea].att;
		       if (rarea > 0) jj = Map.Area[rarea].att;

		            /* put out the attribute info for this area */
                       printf( "\t   area attribute:\n       type= %c,  cat= %6d,  E= %10.2lf, N=  %10.2lf\n", 
                           codes(Map.Att[jj].type),Map.Att[jj].cat,
				 Map.Att[jj].x, Map.Att[jj].y);
		       }
		     }     /* end for islands */

/* --------------------- Area Attributes Section ---------------------- */
                  if ((Full || Attr) && Map.Area[area_num].att)  
		     {      /* put out the attribute info for this area */
                     ii = Map.Area[area_num].att;
                     if (Attr)
			 printf( "   area %ld attribute:\n       type= %c,  cat= %6d,  E= %10.2lf, N=  %10.2lf\n", 
                           area_num,codes(Map.Att[ii].type),Map.Att[ii].cat,
				 Map.Att[ii].x, Map.Att[ii].y);
                     else
			 printf( "   area attribute:\n       type= %c,  cat= %6d,  E= %10.2lf, N=  %10.2lf\n", 
                           codes(Map.Att[ii].type),Map.Att[ii].cat,
				 Map.Att[ii].x, Map.Att[ii].y);
		     }
                  } /* end for Map.Areas */

	     dig_P_fini (&Map);
             fprintf(stderr,"\nFinished <%s>\n",input);

           } else

/* error in usage ? */
	      {
              fprintf(stderr,"\nUsage: snoop [-FALIC] <input_file>\n");
	      fprintf(stderr,"    -F   full report\n");
	      fprintf(stderr,"    -A   areas only report\n");
	      fprintf(stderr,"    -L   lines only report\n");
	      fprintf(stderr,"    -I   islands only report\n");
	      fprintf(stderr,"    -C   categories only report\n\n");
              exit (1);
	      }
}

codes (type)
    char type;
{
    switch (type) {
	case LINE:
	    return ('L');
	    break;
	case AREA:
	    return ('A');
	    break;
	case DOT:
	    return ('P');
	    break;
	case DEAD_LINE:
	    return ('l');
	    break;
	case DEAD_AREA:
	    return ('a');
	    break;
	case DEAD_DOT:
	    return ('p');
	    break;
	default:
	    return ('X');
    }
}
