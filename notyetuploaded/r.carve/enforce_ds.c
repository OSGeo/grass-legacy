/* Written by Bill Brown, UIUC GIS Laboratory
 */

#include "enforce.h"

#define USE_LOWEST

/* for more readability, avoiding void pointer syntax */


/******************************************************************
 * Returns 0 on success, -1 on error, 1 on warning, writing message
 * to errbuf for -1 or 1
*/

enforce_downstream(infile, outfile, outsite, Map, rtype, width, depth, 
		   noflat, errbuf, bequiet)
    FILEDESC infile, outfile;
    char *outsite;
    struct Map_info *Map;
    RASTER_MAP_TYPE rtype;
    double width, depth;
    char *errbuf;
    int bequiet, noflat;
{
struct Cell_head  wind;
int i, j, k, ret, retval, pgpi;
int row, col, inorder;
int prow, pcol;
double elev, width2, dist, totdist;
int line;
int out,in_out,do_warn,first_in;

int npts, status;
PointGrp pg;
PointGrp pgxy;   /* copy any points in region to this one */
Point2 pt, ptxy, *pgpts, *pgxypts;

void *data;
struct line_pnts *Points;
CELL *inrast, *outrast, *cbuf;
FCELL *finrast, *foutrast, *fbuf;
DCELL *dinrast, *doutrast, *dbuf;
struct BM *bm;



    retval = 0;
    width /= 2.0; 
    /* width used here is actually distance to center of stream */

    G_get_set_window (&wind);

    bm = BM_create(wind.cols, wind.rows);
    Points = Vect_new_line_struct ();
    Vect_set_constraint_region (Map, wind.north, wind.south,
				wind.east, wind.west);
    /* first read whole elevation file into buf */
    switch (rtype){
	case CELL_TYPE:
	    if( NULL == (cbuf = 
		(CELL *)malloc(wind.rows*wind.cols*sizeof(CELL)))){
		sprintf(errbuf,"Unable to allocate map.");
		return (-1);
	    }
	    if(!bequiet)
		fprintf(stderr,"Reading CELL_TYPE raster file... ");
	    for(i=0; i<wind.rows; i++){
		if(!bequiet)
		    G_percent (i+1, wind.rows, 10);
		G_get_c_raster_row(infile, &(cbuf[i*wind.cols]), i);
	    }
	    break;
	case FCELL_TYPE:
	    if( NULL == (fbuf = 
		(FCELL *)malloc(wind.rows*wind.cols*sizeof(FCELL)))){
		sprintf(errbuf,"Unable to allocate map.");
		return (-1);
	    }
	    if(!bequiet)
		fprintf(stderr,"Reading FCELL_TYPE raster file... ");
	    for(i=0; i<wind.rows; i++){
		if(!bequiet)
		    G_percent (i+1, wind.rows, 10);
		G_get_f_raster_row(infile, &(fbuf[i*wind.cols]), i);
	    }
	    break;
	case DCELL_TYPE:
	    if( NULL == (dbuf = 
		(DCELL *)malloc(wind.rows*wind.cols*sizeof(DCELL)))){
		sprintf(errbuf,"Unable to allocate map.");
		return (-1);
	    }
	    if(!bequiet)
		fprintf(stderr,"Reading DCELL_TYPE raster file... ");
	    for(i=0; i<wind.rows; i++){
		if(!bequiet)
		    G_percent (i+1, wind.rows, 10);
		G_get_d_raster_row(infile, &(dbuf[i*wind.cols]), i);
	    }
	    break;
    }
    
    /* leave for now - may want to eliminate above code eventually and
       somehow figure out how to write random, so would need to do this
       row by row at that point */
    switch (rtype){
	 case CELL_TYPE:
	     inrast = G_allocate_c_raster_buf();
	     outrast = G_allocate_c_raster_buf();
	     break;
	 case FCELL_TYPE:
	     finrast = G_allocate_f_raster_buf();
	     foutrast = G_allocate_f_raster_buf();
	     break;
	 case DCELL_TYPE:
	     dinrast = G_allocate_d_raster_buf();
	     doutrast = G_allocate_d_raster_buf();
	     break;
	 default:
	     return (-1);
    }
    
    line = 0;
    while (0 <= (ret = Vect_read_next_line (Map, Points))) {
	if (ret != LINE) continue;

	line++;
	npts = 0;

	pg_init(&pg);
	pg_init(&pgxy);
	for(i=0;i<wind.rows;i++)
	    for(j=0;j<wind.cols;j++)
		BM_set (bm, j, i, 0);

	if(!bequiet)
	   fprintf(stderr,"\nline %d: ",line);

	prow = -1;
	totdist = 0.0;
	in_out = do_warn = 0;
	first_in = -1;
	for (i = 0 ; i < Points->n_points ; i++){

	    if(!bequiet)
		G_percent (i+1, Points->n_points, 10);
	    row = G_northing_to_row (Points->y[i], &wind);
	    col = G_easting_to_col (Points->x[i], &wind);

	    /* rough clipping */
	    if(row < 0 || row > wind.rows-1 || col < 0 || col > wind.cols-1){
		if(first_in != -1)
		    in_out = 1;
                continue;
	    }
	    if(first_in < 0)
		first_in = i;
            else if (in_out)
		do_warn = 1;
	    ptxy[0] = Points->x[i];
	    ptxy[1] = Points->y[i];

#ifdef USE_LOWEST
	    switch (rtype){
		case CELL_TYPE:
		    data = (void *)cbuf;
		    break;
		case FCELL_TYPE:
		    data = (void *)fbuf;
		    break;
		case DCELL_TYPE:
		    data = (void *)dbuf;
		    break;
	    }
	    pt[1] = lowest_cell_near_point(data, rtype, &wind, 
		    ptxy[0], ptxy[1], width);
#else
	    switch (rtype){
		case CELL_TYPE:
		    if(row != prow) 
			G_get_c_raster_row(infile, inrast, row);
		    pt[1] = inrast[col];
		    break;
		case FCELL_TYPE:
		    if(row != prow) 
			G_get_f_raster_row(infile, finrast, row);
		    pt[1] = finrast[col];
		    break;
		case DCELL_TYPE:
		    if(row != prow) 
			G_get_d_raster_row(infile, dinrast, row);
		    pt[1] = dinrast[col];
		    break;
	    }
	    prow = row;
#endif

	    if(i){
		dist = G_distance(Points->x[i-1], Points->y[i-1],
				  Points->x[i], Points->y[i]);
		totdist += dist;
	    }
	    pt[0] = totdist;
	    pg_addpt(&pg, pt);
	    pg_addpt(&pgxy, ptxy);
	    npts++;
	}
	if(do_warn){
		sprintf (errbuf, "Vect runs out of region and re-enters - \n");
		strcat  (errbuf, "this case not yet implemented!");
		retval = 1;
	}
	/* TODO: check for NULL */
	/* now check to see if points go downslope(inorder) or upslope */
	inorder = (pg_y_from_x(&pg, 0.0) > pg_y_from_x(&pg, totdist));

	if(inorder){
	    pgpts = pg_getpoints(&pg);
	    pgxypts = pg_getpoints(&pgxy);
	}
	else{
	    pgpts = pg_getpoints_reversed(&pg);  /* pgpts is now high to low */
	    for (i = 0 ; i < npts ; i++)
		pgpts[i][0] = totdist - pgpts[i][0];
	    pgxypts = pg_getpoints_reversed(&pgxy);  
	}
	for (i = 0 ; i < (npts - 1) ; i++){
	    if(noflat){
		if(pgpts[i+1][1] < pgpts[i][1]) 
		    continue;
		for(j=(i+2); j < npts; j++){
		    if(pgpts[j][1] < pgpts[i][1]) break; 
		}
		/* if we got to the end, lower end by depth OR .01 & INTERP */
		if(j == npts){
		    --j;
		    pgpts[j][1] = pgpts[i][1] - (depth > 0? depth : .01);
		    for(k=(i+1); k < j; k++){
			pgpts[k][1] = LINTERP(pgpts[j][1],pgpts[i][1],
				 (pgpts[j][0]-pgpts[k][0])
				 /(pgpts[j][0]-pgpts[i][0]));
		    }
		}
		/* otherwise, linear interp between point i and the next < */
		else{
		    for(k=(i+1); k < j; k++){
			pgpts[k][1] = LINTERP(pgpts[j][1],pgpts[i][1],
				 (pgpts[j][0]-pgpts[k][0])
				 /(pgpts[j][0]-pgpts[i][0]));
		    }
		}
	    }
	    else{
		if(pgpts[i+1][1] <= pgpts[i][1]) 
		    continue;
		for(j=(i+2); j < npts; j++){
		    if(pgpts[j][1] <= pgpts[i][1]) break; 
		}
		/* if we got to the end, level it out */
		if(j == npts){
		    for(j=(i+1); j < npts; j++){
			pgpts[j][1] = pgpts[i][1];
		    }
		}
		/* otherwise, linear interp between point i and the next <= */
		else{
		    for(k=(i+1); k < j; k++){
			pgpts[k][1] = LINTERP(pgpts[j][1],pgpts[i][1],
				 (pgpts[j][0]-pgpts[k][0])
				 /(pgpts[j][0]-pgpts[i][0]));
		    }
		}
	    }
	}
	/* points are now in order and adjusted to run high to
	   low with no dips or mounds - if site output file given,
	   write it now */
        if(outsite)
	    write_xyz_sites(pgxypts, pgpts, npts, outsite, depth);

	/* Now for each segment in the line, use
	 * distance from segment to find beginning row from
	 * northernmost point, beginning col from easternmost,
	 * ending row & col, then loop through bounding box
	 * and use distance from segment to emboss new elevations 
	*/
	{
	int r,c,row1,row2,col1,col2,rowoff,coloff;
	double cellx, celly, cy;

	/* kludge - fix for lat_lon */
	rowoff = width/wind.ns_res ;
	coloff = width/wind.ew_res ;

	width2 = width * width;
/*
if(!inorder){
fprintf(stderr,"skipping... low to high - not yet implemented\n");
continue;
}
* should be OK now
*/
	for (i = 0 ; i < (npts - 1) ; i++){
	    row = G_northing_to_row (pgxypts[i][1], &wind);
	    col = G_easting_to_col (pgxypts[i][0], &wind);
	    if (i){
		row1 = MAX(0, MIN(row,prow) - rowoff);
		row2 = MIN(wind.rows - 1, MAX(row,prow) + rowoff);
		col1 = MAX(0, MIN(col,pcol) - coloff);
		col2 = MIN(wind.cols - 1, MAX(col,pcol) + coloff);
/* debug
fprintf(stdout,"bbox = %d,%d  %d,%d\n", col1,row1,col2,row2);
*/
		for (r = row1; r < row2; r++){
		    cy = G_row_to_northing(r+0.5, &wind);
		    for (c = col1; c < col2; c++){
			cellx = G_col_to_easting(c+0.5, &wind);
			celly = cy;  /* gets written over in distance2... */

/* Thought about not going past endpoints (use status to check)
 * but then pieces end up missing from outside corners - if it goes
 * past ends, should probably do some interp or will get flats */
/* here we use a bitmap and only change cells once on the way down */
			if(width2 >= 
			   xy_distance3_point_to_seg(&cellx,&celly,
			   pgxypts[i-1][0],pgxypts[i-1][1],
			   pgxypts[i][0],pgxypts[i][1], &status)){
			     if( !BM_get(bm, c, r) ){
				BM_set(bm, c, r, 1);
				elev = LINTERP(pgpts[i][1],pgpts[i-1][1],
				    G_distance(pgxypts[i][0],pgxypts[i][1],
				    cellx,celly)/(pgpts[i][0]-pgpts[i-1][0]));

/* TODO - may want to use a function for the cross section of stream */
				switch (rtype){
				    case CELL_TYPE:
					cbuf[r*wind.cols+c] = 
					    MIN(cbuf[r*wind.cols+c],elev) - 
						depth; 	
					break;
				    case FCELL_TYPE:
					fbuf[r*wind.cols+c] = 
					    MIN(fbuf[r*wind.cols+c],elev) - 
						depth; 	
					break;
				    case DCELL_TYPE:
					dbuf[r*wind.cols+c] =
					    MIN(fbuf[r*wind.cols+c],elev) - 
						depth; 	
					break;
				 }
			     }
			}
		    }
		}
	    }
	    prow = row;
	    pcol = col;
	}
	}
    }
/* debug 
fprintf(stderr," inorder: %d  %.3lf %.3lf\n",
inorder, pg_y_from_x(&pg, 0.0), pg_y_from_x(&pg, totdist));
*/         
    if(!bequiet)
	fprintf(stderr,"\nWriting file... ");
    switch (rtype){
	case CELL_TYPE:
	    for(i=0; i<wind.rows; i++){
		if(!bequiet)
		    G_percent (i, wind.rows, 10);
		G_put_c_raster_row(outfile, cbuf+i*wind.cols);
	    }
            break;
	case FCELL_TYPE:
	    for(i=0; i<wind.rows; i++){
		if(!bequiet)
		    G_percent (i, wind.rows, 10);
		G_put_f_raster_row(outfile, fbuf+i*wind.cols);
	    }
            break;
	case DCELL_TYPE:
	    for(i=0; i<wind.rows; i++){
		if(!bequiet)
		    G_percent (i, wind.rows, 10);
		G_put_d_raster_row(outfile, dbuf+i*wind.cols);
	    }
            break;
    }
    Vect_destroy_line_struct(Points);
    BM_destroy(bm);
    switch (rtype){
	case CELL_TYPE:
	    G_free(inrast);
	    G_free(outrast);
	    G_free(cbuf);
            break;
	case FCELL_TYPE:
	    G_free(finrast);
	    G_free(foutrast);
	    G_free(fbuf);
            break;
	case DCELL_TYPE:
	    G_free(dinrast);
	    G_free(doutrast);
	    G_free(dbuf);
            break;
    }

    if(!bequiet)
	fprintf(stderr,"Done.\n");

    return(retval);


}

/***********************************************
 * returns the lowest value cell within radius rad of px, py 
*/

double
lowest_cell_near_point(data, rtype, wind, px, py, rad)
void *data;
RASTER_MAP_TYPE rtype;
struct Cell_head  *wind;
double px, py, rad;
{
int r,c,row,col,row1,row2,col1,col2,rowoff,coloff;
double min, cellx, celly, cy, rad2;
CELL *cbuf;
FCELL *fbuf;
DCELL *dbuf;

    /* kludge - fix for lat_lon */
    rowoff = rad/wind->ns_res ;
    coloff = rad/wind->ew_res ;

    rad2 = rad * rad;

    row = G_northing_to_row (py, wind);
    col = G_easting_to_col (px, wind);
    row1 = MAX(0, row - rowoff);
    row2 = MIN(wind->rows - 1, row + rowoff);
    col1 = MAX(0, col - coloff);
    col2 = MIN(wind->cols - 1, col + coloff);
    switch (rtype){
	case CELL_TYPE:
	    cbuf = (CELL *)data;
	    min = cbuf[row1*wind->cols+col1];
	    break;
	case FCELL_TYPE:
	    fbuf = (FCELL *)data;
	    min = fbuf[row1*wind->cols+col1];
	    break;
	case DCELL_TYPE:
	    dbuf = (DCELL *)data;
	    min = dbuf[row1*wind->cols+col1];
	    break;
     }
    for (r = row1; r < row2; r++){
	cy = G_row_to_northing(r+0.5, wind);
	for (c = col1; c < col2; c++){
	    cellx = G_col_to_easting(c+0.5, wind);
	    celly = cy;  /* gets written over in distance2... */

	    if(rad2 >= G_distance(px, py, cellx, celly)) {

		    switch (rtype){
			case CELL_TYPE:
			    if (cbuf[r*wind->cols+c] < min)
				min = cbuf[r*wind->cols+c];
			    break;
			case FCELL_TYPE:
			    if (fbuf[r*wind->cols+c] < min)
				min = fbuf[r*wind->cols+c];
			    break;
			case DCELL_TYPE:
			    if (dbuf[r*wind->cols+c] < min)
				min = dbuf[r*wind->cols+c];
			    break;
		     }
	    }
	 }
    }
    return(min);
}


/********************* JUNK ***********************/
#ifdef JUNK

/* TEMP - to test, write to sites file 
   No - s.to.rast doesn't work in 4.2 */
for(i = 0; i< Points->n_points ; i++){
   fprintf(stdout, "%f|%f|%%%f\n", Points->y[i],
	   Points->x[i], pgpts[i][1]);
}

#endif
