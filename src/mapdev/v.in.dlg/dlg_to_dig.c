
/*  DLG_TO_DIG     convert BDLG format files into digit files for editing.
**     This process will remove labels from the data.   The dlg_label
**	program   can be used to save labels and restore them after editting.
*/

/* if area exists and has attributes, then consider its lines as area lines
**  Have to deal with Area 1.  Digit data should not normally  have an A 1 type 
**  boundry.  We add it in dig_to_dlg as a bounding box that has 1 line and does
**  NOT intersect with any lines in data.   A 1 should also have attributes 
**  000 0000.  This means that A2 on one of our processed dlg files will 
**  have an A2 that is == -A1.  On dlg files that we get in from the out-
**  side world, anything goes.  It is possible that A1 could be made up  
**  of lines from the data, and I dont see a way to guarantee that we are
**  not throwing away valid data if we remove A1.
**
**  Algorithm: 
**	If A1 has 1 and only 1 line  AND A1 has no attrs or attrs 000 0000
**         AND  A2 == -A1,   then remove the line in A1.
**
**  Result:  
**	On files we have created w/ digit and dig_to_dlg, This will remove
**      A1 and A2 when going back to dig. These will be replace again in
**	dig_to_dlg.
**
**	On files that come in from outside, if they are similar to ours, then
**	A1 and A2 will be removed, otherwise nothing will be removed and
** 	dig_to_dlg will add another box around the data.  The editor in digit
**	is of course free to remove any box that is not real data.
*/
/*
**  Written by Dave Gerdes and Mike Higgins 4/1988
**  US Army Construction Engineering Research Lab
*/


/*  POINT is also known as DOT */

#include "dlg.h"
#include "ibtree.h"
#include <stdio.h>
#include "Vect.h"
#include "gis.h"
#include "dig_atts.h"
#define COOR_MAX		5000
#define ALLOC_AMT               512	
#define MAXLINE		90
#define CHAR_NULL	'\0'
#define FGET 		if (fgets(buff,MAXLINE,fd) == NULL) strcpy(buff,"E")
#define BOUND    8
#define ATT_AREA 'A'
#define ATT_LINE 'L'
#define ATT_POINT 'P'

extern struct dlg dlgstr;


int compare (Tkey,key)
  int Tkey;
  int key;
{
  if (Tkey==key) return 0;
  if (Tkey > key) return 1;
  else return -1;
} 




dlg_to_dig( fd, map, f_att, add_att_name, off_att, Force_lines)
	FILE *fd ;
	struct Map_info *map;
	FILE *f_att ;
        char *add_att_name;
        int  off_att;
        int Force_lines;
{
    char type ;
    int  itype ;
    int  addcond;
    FILE *add_att;
    struct line_pnts *Points;
    struct dlg_node  node;
    struct dlg_line  line;
    struct dlg_area  area;
    IBTREE *B;
    int   (*cmp)();

    char buff[128] ;
    double x ;
    double y ;

    int n_area_lines ;

    int num ;
    int n_read ;

    int num_nodes = 0;
    int num_areas = 0;
    int num_lines = 0;
    int num_undef = 0;

    int n,i ;
    int rem ;
    int area_lines ;

    int last_n = 1;
    int last_nl = 1;
    int one_line = 0;
    int szint = sizeof(int);

    int step ;
    int degenerate = 0;
    int status ;

    double *xarray ;
    double *yarray ;

    int cur_ident = off_att;
    int j;


#ifndef abs
#define abs(x) ((x) >= 0 ? (x) : -(x))
#endif

    if (add_att_name != NULL) addcond = 1;
     else addcond = 0;

    if (addcond) {
      if (!(add_att = fopen(add_att_name,"w")))  {
        fprintf(stderr,"Cannot open %s for writing\n",add_att_name);
        exit (0);
      }
    }

    cmp = compare;
    if(!(B = (IBTREE *) malloc(sizeof(IBTREE)))) {
      fprintf(stderr,"Cannot allocate tree\n");
      exit (0);
    }

    if (!(ibtree_create(B,cmp,1))) {
      fprintf(stderr,"Cannot create IBTREE\n");
      exit (0);
    }
    Points = Vect_new_line_struct();

    if(!(node.lines = (int *) malloc(sizeof(int)*COOR_MAX))) {
      fprintf(stderr,"Cannot allocate node.lines\n");
      exit (0);
    }
    if (!(node.atts = (int *) malloc(sizeof(int)*COOR_MAX))) {
      fprintf(stderr,"Cannot allocate node.atts\n");
      exit (0);
    }
    if(!(area.lines = (int *) malloc(sizeof(int)*COOR_MAX))) {
      fprintf(stderr,"Cannot allocate are.lines\n");
      exit (0);
    }
    if (!(area.atts = (int *) malloc(sizeof(int)*COOR_MAX)))  {
      fprintf(stderr,"Cannot allocate are.atts\n");
      exit (0);
    }
    if(!(line.atts = (int *) malloc(sizeof(int)*COOR_MAX)))  {
      fprintf(stderr,"Cannot allocate line.atts\n");
      exit (0);
    }
    if (!(line.coors = (double *) malloc(sizeof(double)*COOR_MAX*2))) {
      fprintf(stderr,"Cannot allocate line.coors\n");
      exit (0);
    }
    for(;;)
    {
	FGET ;
	switch (*buff)
	{
	    case 'N':
		num_nodes++ ;
		/* dpg */
		scan_node_record(buff,&num,&x,&y,&(node.n_lines),&(node.n_atts)) ;

		if (node.n_lines)
		    if (n_read = read_int(fd, node.n_lines, node.lines))
		    {
			printf("Error: Missing %d lines for node %d\n",
			n_read, num) ;
			node.n_lines -= n_read ;
		    }
		if (node.n_atts)
		    if (n_read = read_int(fd, node.n_atts * 2, node.atts) )
		    {
			printf("Error: Missing %d attributes for area %d\n",
			n_read, num) ;
			node.n_atts -= n_read / 2 ;
		    }

		/**  take care of different parameters  */
		node.x = dlgstr.proj.int_params[0] * x
	 	   + dlgstr.proj.int_params[1] *y 
	 	   + dlgstr.proj.int_params[2] ; 

		node.y = dlgstr.proj.int_params[0] * y 
	 	   + dlgstr.proj.int_params[1] * x 
	 	   + dlgstr.proj.int_params[3] ; 

/*       WRITE NODE HERE       */

		/*dpg*/
		/* if have a Site, then write a degen line */
                 if ((node.n_lines == 0) || (node.n_atts > 0))
		    dig_write_point (map, Points, &(node.x),
			    &(node.y), DOT) ;

		/*  have a valid attribute??  */

		/*  Change by D. Satnik to allow negative atts
		if (node.n_atts <= 0  ||  node.atts[1] <= 0)
			continue ;
		*/
		if (node.n_atts <= 0  ||  node.atts[1] != 0)
			continue ;

                if (addcond) {
                  if (node.n_atts > 0) {
                    fprintf(add_att,"%d       %d         %d\n",cur_ident,
                                               node.atts[0],node.atts[1]);
                    for (j=2;j<node.n_atts*2-1;j+=2) {
                      fprintf(add_att,"        %d         %d\n",
                                               node.atts[j],node.atts[j+1]);
                    }
    		    write_att (f_att, FILE_DOT, node.x,
		      node.y, cur_ident) ;
                    cur_ident ++;
                  }
                }
                else {
		/*  save Node label as a dig Point  */
    		  write_att (f_att, FILE_DOT, node.x,
			node.y, node.atts[1]) ;
                }
		break ;

	    case 'A':
		num_areas++ ;
		area.n_atts = 0 ;	/* in case there are no values  */
		area.n_isles = 0 ;
		n_area_lines = 0 ;
		scan_area_record( buff, &num, &x, &y,
	          &(area.n_lines),&n_area_lines,&(area.n_atts),&(area.n_isles));

		if (area.n_lines)
		    if (n_read = read_int(fd,area.n_lines,area.lines) )
		    {
			printf("Error: Missing %d lines for area %d\n",
			n_read, num) ;
			area.n_lines -= n_read ;
		    }

		/*
		* Calculate number of lines of area-line coordinates
		* and skip over that number of lines.
		*/
		if (n_area_lines)
		{
		    n = n_area_lines / 3 ;
		    rem = n_area_lines % 3 ;
		    area_lines =  rem ? ++n : n ;
		    for ( n = 0 ; n < area_lines ; ++n)
			FGET ;
		}

		if (area.n_atts)
		    if (n_read = read_int(fd,area.n_atts * 2,area.atts))
		    {
			printf("Error: Missing %d attributes for area %d\n",
			n_read, num) ;
			area.n_atts -= n_read / 2 ;
		    }

		/**  take care of different parameters  */
		area.x = dlgstr.proj.int_params[0] * x
	 	   + dlgstr.proj.int_params[1] * y 
	 	   + dlgstr.proj.int_params[2] ; 

		area.y = dlgstr.proj.int_params[0] * y 
	 	   + dlgstr.proj.int_params[1] * x 
	 	   + dlgstr.proj.int_params[3] ; 

/*       WRITE AREA HERE       */

	if (area.n_atts || !Force_lines)
	{

		/*  Area 1, is it one line (universe box)  */
		if ( num_areas == 1 && area.n_lines == 1)
		    if (area.n_atts == 0 || (area.n_atts == 1 &&
			area.atts[0] == 0 && area.atts[1] == 0))
			    one_line = area.lines[0] ;

		/*  lines making up area set to Area types.

		**  check only lines making up area and do not check
		**  island lines  added 7/88, The Digits
		*/
		for (i = 0 ; i < area.n_lines ; i++)
/**/		    if (area.lines[i] == 0)
/**/			break;
/**/		    else
                    {

                      if (!(ibtree_update(B,abs(area.lines[i]),AREA))) 
                        fprintf(stderr,"cannot update tree\n");
                    }

		/*  one line makes up a useless bounding box  */
		if ( num_areas == 2 && one_line  &&  area.lines[0] == -one_line)
/*		    linetypes[abs(one_line)] = BOUND; */
                if (!(ibtree_update(B,abs(one_line),BOUND))) 
                   fprintf(stderr,"cannot update tree\n");
                if (addcond) {
                  if (area.n_atts >0) {
		    if(!(area.atts[0] == 0 && area.atts[1] == 0)) {
                      fprintf(add_att,"%d       %d         %d\n",cur_ident,
                                               area.atts[0],area.atts[1]);
                      for (j=2;j<area.n_atts*2-1;j+=2) {
                        fprintf(add_att,"        %d         %d\n",
                                               area.atts[j],area.atts[j+1]);
                      }
    		      write_att (f_att, FILE_AREA, area.x,
		        	area.y, cur_ident) ;
                      cur_ident ++;
                    }
                  }
                }
                else {
		/*  if there is an Area label save it  */
        	  if (area.n_atts > 0  &&  area.atts[1] > 0)
	    		write_att (f_att, FILE_AREA, area.x, area.y,
			area.atts[1]) ;
                }

	}
                last_n++;


		break ;

	    case 'L':
		num_lines++ ;
		line.n_atts = 0 ;
		sscanf(buff, "%*1c%5d%6d%6d%6d%6d%*12c%6d%6d",
		    &num, &(line.start_node), &(line.end_node),
		    &(line.left_area), &(line.right_area), 
                    &(line.n_coors), &(line.n_atts) ) ;



		if (line.n_coors > COOR_MAX)
		{
		    fprintf (stderr, "ERROR: Too many coordinates for a single line.   L %d\n", num) ;
		    exit (-1) ;
		}

		if (line.n_coors)
		{
		    if (n_read = read_doubles(fd,line.n_coors * 2,line.coors))
		    {
			printf("Error: Missing %d coordinates for line %d\n",
			    n_read, num) ;
			line.n_coors -= n_read / 2 ;
		    }
		}
		else
		{
		    line.N = 0.0 ;
		    line.S = 0.0 ;
		    line.E = 0.0 ;
		    line.W = 0.0 ;
		}

		if (line.n_atts)
		    if (n_read = read_int(fd,line.n_atts * 2,line.atts))
		    {
			printf("Error: Missing %d attributes for line %d\n",
			n_read, num) ;
			line.n_atts -= n_read / 2 ;
		    }

/*       WRITE LINE HERE       */

		if (line.n_coors < 2)
			continue ;
	   	breakout_xy ( line.coors, line.n_coors, &xarray, &yarray) ;

/*		type = linetypes[n] ; */

                type  = LINE;
                if (ibtree_find(B,abs(num),&itype) == 1)  {
                  type = itype;
                }
		if (type == BOUND)
		{
			continue ;
		}

		/**  if degenerate line,  toss **/
		if (line.n_coors == 2
			&&  xarray[0]  ==  xarray[1]
			&&  yarray[0]  ==  yarray[1])
		{
			++degenerate ;
			continue ;
		}


   		dig_write_line (map, Points, xarray, yarray,
			line.n_coors, type == AREA ? AREA : LINE);
                if (addcond) {
                  if (line.n_atts > 0) {
                    fprintf(add_att,"%d       %d         %d\n",cur_ident,
                                               line.atts[0],line.atts[1]);
                    for (j=2;j<line.n_atts*2-1;j+=2) {
                      fprintf(add_att,"        %d         %d\n",
                                               line.atts[j],line.atts[j+1]);
                    }
   	  	    write_att_line (f_att, xarray, yarray, line.n_coors,
				cur_ident) ;
                    cur_ident ++;
                  }
                }
                else {
		/*  valid non-zero attribute  */
    	   	if (line.n_atts > 0  &&  line.atts[1] > 0)
   			write_att_line (f_att, xarray, yarray, line.n_coors,
				line.atts[1]) ;
                }
                last_nl ++;

		break ;

	    case 'E':
		fprintf (stderr,"\n") ;
		fprintf (stderr,"    nodes: %d\n", num_nodes) ;
		fprintf (stderr,"    areas: %d\n", num_areas) ;
		fprintf (stderr,"    lines: %d\n", num_lines) ;
		fprintf (stderr,"  unknown: %d\n", num_undef) ;
		fprintf (stderr,"\n") ;
                if (degenerate)
                  fprintf (stderr,"    degenerate lines %d\n", degenerate) ;
                Vect_destroy_line_struct (Points);
                if (addcond) fclose(add_att);
		return(0) ;

	    default:
		printf(" unknown line: '%s',n=%d\n", buff,num_lines) ;
	    num_undef++ ;
	}
    }
    return 1;
}

/*  This strips out the Area or Line linkage record from a string  */

static  
scan_node_record (str, num, x, y, n_lines, n_atts)
    char *str ;
    int  *num ;
    double  *x, *y ;
    int  *n_lines ;
    int  *n_atts ;

{

    char  char_num[6] ;
    char  char_x[13] ;
    char  char_y[13] ;
    char  char_lines[7] ;
    char  char_atts[7] ;

    int  atoi() ;
    double  atof() ;


    sscanf(str, "%*1c%5c%12c%12c%*6c%6c%*6c%6c",
		    char_num, char_x, char_y, char_lines, char_atts) ;

    /*  make sure there null terminated */
    char_num[5] = CHAR_NULL ;
    char_x[12] = CHAR_NULL ;
    char_y[12] = CHAR_NULL ;
    char_lines[6] = CHAR_NULL ;
    char_atts[6] = CHAR_NULL ;


    *num = atoi(char_num) ;
    *x = atof(char_x) ;
    *y = atof(char_y) ;
    *n_lines = atoi(char_lines) ;
    *n_atts = atoi(char_atts) ;
}

static 
scan_area_record (str, num, x, y, n_lines, n_area_lines, n_atts, n_isles)
    char *str ;
    int  *num ;
    double  *x, *y ;
    int  *n_area_lines, *n_lines, *n_atts, *n_isles ;
{

    char  char_a_lines[7] ;
    char  char_atts[7] ;
    char  char_isles[7] ;
    int junk;

    int  atoi() ;

    /*  the area and node records are the same up to a point */
    scan_node_record( str, num, x, y, n_lines, n_atts) ;

    /*  skip the info the scan_node_record() had gotten  */
    sscanf(str, "%*42c%6c%6c%*6c%6c",
    char_a_lines, char_atts, char_isles) ;

    /*  make sure there null terminated */
    char_a_lines[6] = CHAR_NULL ;
    char_atts[6] = CHAR_NULL ;
    char_isles[6] = CHAR_NULL ;

    *n_area_lines = atoi(char_a_lines) ;
    *n_atts = atoi(char_atts) ;
    *n_isles = atoi(char_isles) ;
}



