#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "conv.h"

int read_line ( FILE *, struct line_pnts *);

/* read old 3.0 or 4.0 dig file into array 
   returns number of elements read into array
   or -1 on error */
int read_dig ( FILE *Digin, struct Map_info *Mapout, 
               struct Line **plines , int endian, int att )
{
    char	      buf[100];
    struct dig_head   In_head;
    int               i, lalloc, line=0, type, portable=1;
    int    npoints=0, nlines=0, nbounds=0;     
    int    ndpoints=0, ndlines=0, ndbounds=0, nunknown=0;         
    double dbuf;
    struct Line *lines;
    struct line_pnts *nline;
    struct line_cats *cat_out;
    
    Vect__init_head (&(In_head));
    In_head.byte_order = endian;
    /* set conversion matrices */
    dig__init_head_portable (&(In_head));

    /* Version 3 dig files were not portable and some version 4 
     * files may be also non portable */

    fprintf(stdout,"Reading dig file...\n");

    /* read and copy head */
    fseek (Digin, 0L, SEEK_SET) ;   /* set to beginning */

    if (0 >= dig__fread_port_C ( buf, DIG4_ORGAN_LEN, Digin)) return -1;
    buf[DIG4_ORGAN_LEN-1] = '\0'; 
    strncpy ( &Mapout->head.organization, &buf, DIG_ORGAN_LEN );

    if (0 >= dig__fread_port_C ( buf, DIG4_DATE_LEN, Digin)) return -1;
    buf[DIG4_DATE_LEN-1] = '\0'; 
    strncpy ( &Mapout->head.date, buf, DIG_DATE_LEN );
    
    if (0 >= dig__fread_port_C ( buf, DIG4_YOUR_NAME_LEN, Digin)) return -1;
    buf[DIG4_YOUR_NAME_LEN-1] = '\0'; 
    strncpy ( &Mapout->head.your_name, &buf, DIG_YOUR_NAME_LEN );

    if (0 >= dig__fread_port_C ( buf, DIG4_MAP_NAME_LEN, Digin)) return -1;
    buf[DIG4_MAP_NAME_LEN-1] = '\0'; 
    strncpy ( &Mapout->head.map_name, &buf, DIG_MAP_NAME_LEN );

    if (0 >= dig__fread_port_C ( buf, DIG4_SOURCE_DATE_LEN, Digin)) return -1;
    buf[DIG4_SOURCE_DATE_LEN-1] = '\0'; 
    strncpy ( &Mapout->head.source_date, &buf, DIG_SOURCE_DATE_LEN );

    if (0 >= dig__fread_port_C ( buf, DIG4_LINE_3_LEN, Digin)) return -1;
    buf[DIG4_LINE_3_LEN-1] = '\0'; 
    strncpy ( &Mapout->head.line_3, buf, DIG_LINE_3_LEN );

    if (0 >= dig__fread_port_C( buf, VERS_4_DATA_SIZE, Digin)) return -1; 

    if (buf[0] != '%' || buf[1] != '%') { /* Version3.0 */
	In_head.Version_Major = 3;
	portable = 0;  /* input vector is not portable format*/
        fprintf(stdout,"Input file is version 3.\n") ;	
    } else {
	In_head.Version_Major = 4;
	fprintf(stdout,"Input file is version 4.\n") ;	
	/* determine if in portable format or not */
	if (buf[6] == 1 && (~buf[6]&0xff) == buf[7]) {   /* portable ? */
	    portable = 1;  /* input vector is portable format*/
	} else {	
	    portable = 0;  /* input vector is not portable format*/
	}
    }
    if ( portable == 1) {
	fprintf(stdout,"Input file is portable.\n") ;
    } else {
	fprintf(stdout,"Input file is not portable.\n
			We will attempt to convert anyway but conversion may fail.\n
			Please read manual for detail information.\n") ;	    
    }
	    
    /* set Cur_Head because it is used by dig__*_convert()
       called by dig__fread_port_*() */
    dig__set_cur_head ( &In_head );

    if (0 >= dig__fread_port_L ( &Mapout->head.orig_scale, 1, Digin)) return -1;
    if (0 >= dig__fread_port_I ( &Mapout->head.plani_zone, 1, Digin)) return -1;
    if (0 >= dig__fread_port_D ( &dbuf, 1, Digin)) return -1;  /* W */
    if (0 >= dig__fread_port_D ( &dbuf, 1, Digin)) return -1; /* E */    
    if (0 >= dig__fread_port_D ( &dbuf, 1, Digin)) return -1; /* S */
    if (0 >= dig__fread_port_D ( &dbuf, 1, Digin)) return -1; /* N */
    if (0 >= dig__fread_port_D ( &Mapout->head.map_thresh, 1, Digin)) return -1;

    /* reading dig file body (elements) */
    nline = Vect_new_line_struct ();
    cat_out = Vect_new_cats_struct();
    
    lalloc = 0;
    lines = NULL;
    
    line = 0;
    while ( (type = read_line ( Digin, nline) )> 0 ) {
	switch (type) {
	    case DOT:
		npoints++;
		break;
	    case LINE:
		nlines++;
		break;
	    case BOUNDARY:
		nbounds++;
		break;
	    case DEAD_DOT:
		ndpoints++;
		break;
	    case DEAD_LINE:
		ndlines++;
		break;
	    case DEAD_BOUNDARY:
		ndbounds++;
		break;
	    defaul:
		nunknown++;
		break;	    
	}
	if ( !(type & (DOT | LINE | BOUNDARY )))  continue; 
	
	if ( (type & BOUNDARY) || !att){
            Vect_write_line ( Mapout, type, nline, cat_out );
            /* reset In_head */
	    dig__set_cur_head ( &In_head );
	} else {   /* DOT or LINE */
	    if ( line >= lalloc ) {
	        lalloc += 10000;
	        lines = (struct Line *) realloc ( lines, lalloc * sizeof(struct Line));
	    }
	    lines[line].type = type;
	    lines[line].n_points = nline->n_points;
	    lines[line].cat = -1;
	    lines[line].x = (double *) malloc ( nline->n_points * sizeof (double));
	    lines[line].y = (double *) malloc ( nline->n_points * sizeof (double));
	    memcpy ( (void *) lines[line].x, (void *) nline->x,
		     nline->n_points * sizeof(double));
	    memcpy ( (void *) lines[line].y, (void *) nline->y,
		     nline->n_points * sizeof(double));
	    line++;
	}
    }
    if (att) {
        fprintf(stdout,"%-5d points read to memory\n", npoints);
        fprintf(stdout,"%-5d lines read to memory\n", nlines);
    } else {
        fprintf(stdout,"%-5d points read and written to output\n", npoints);
        fprintf(stdout,"%-5d lines read and written to output\n", nlines);
    }
    fprintf(stdout,"%-5d area boundaries read and written to output\n", nbounds);    
    fprintf(stdout,"%-5d dead points skipped\n", ndpoints);
    fprintf(stdout,"%-5d dead lines skipped\n", ndlines);    
    fprintf(stdout,"%-5d dead area boundaries skipped\n", ndbounds);        
    fprintf(stdout,"%-5d elements of unknown type skipped\n", nunknown);

    fprintf(stdout,"%-5d elements read to memory.\n", line);

    *plines = lines;
    return (line);
}

/* read_line() reads element from file
   returns element type
 */   
int read_line ( FILE *Digin, struct line_pnts *nline )
{
    int n_points;
    long itype;

    if (0 >= dig__fread_port_L (&itype, 1, Digin)) return (0);
    itype = dig_old_to_new_type ((char) itype);

    if (0 >= dig__fread_port_I (&n_points, 1, Digin)) return (0);
 
    if (0 > dig_alloc_points ( nline, n_points)) return (-1);
 
    nline->n_points = n_points;
    if (0 >= dig__fread_port_D ( nline->x, n_points, Digin)) return (0);
    if (0 >= dig__fread_port_D ( nline->y, n_points, Digin)) return (0);    

    return ((int) itype);
}  

/* read old 3.0, 4.0 dig_att file into array*/
int read_att (FILE *Attin, struct Categ **pcats )
{
    int ctalloc=0, cat=0, att, type, ret, rcat; 
    int    npoints=0, nlines=0, ncentroids=0;     
    int    ndpoints=0, ndlines=0, ndcentroids=0, nunknown=0;         
    char buf[201], ctype;
    double x, y;
    struct Categ *cats;
    
    fprintf(stdout,"Reading dig_att file...\n");
    
    ctalloc = 0;
    cats = NULL;

    while ( fgets( buf, 200, Attin ) != NULL) {
        ret = sscanf( buf, "%c %lf %lf %d", &ctype, &x, &y, &rcat);
        if (ret != 4) { 
            fprintf(stderr,"Error: %s\n", buf);
	    continue;
	}
	switch (ctype) {
	    case 'P':
		type = DOT;
		npoints++;
		break;
	    case 'L':
		type = LINE;
		nlines++;
		break;
	    case 'A':
		type = CENTROID;
		ncentroids++;
		break;
	    case 'p':
		type = DEAD_DOT;
		ndpoints++;
		break;
	    case 'l':
		type = DEAD_LINE;
		ndlines++;
		break;
	    case 'a':
		type = DEAD_CENTROID;
		ndcentroids++;
		break;
            defaut:
                fprintf(stderr,"Unknown type: %c\n", ctype);
		type = 0;
		nunknown++;
                break;
        }
	if (!(type & (DOT | LINE | CENTROID))) { continue;} 
			
	if ( cat >= ctalloc ) {
	    ctalloc += 10000;
	    cats = (struct Categ *) realloc ( cats, ctalloc * sizeof(struct Categ));
	}
        cats[cat].type = type;         
        cats[cat].x = x;         
        cats[cat].y = y;         
        cats[cat].cat = rcat;         
        cat++;
	
    }
    
    fprintf(stdout,"%-5d point categories read\n", npoints);
    fprintf(stdout,"%-5d line categories read\n", nlines);    
    fprintf(stdout,"%-5d centroids read\n", ncentroids);    
    fprintf(stdout,"%-5d dead point categories skipped\n", ndpoints);
    fprintf(stdout,"%-5d dead line categories skipped\n", ndlines);    
    fprintf(stdout,"%-5d dead centroids skipped\n", ndcentroids);        
    fprintf(stdout,"%-5d categories of unknown type skipped\n", nunknown);
    
    fprintf(stdout,"%-5d categories read into memory.\n", cat);
    
    *pcats = cats; 
    return (cat);    
}


