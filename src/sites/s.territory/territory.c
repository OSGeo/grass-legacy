#include <stdlib.h>
#include "gis.h"
#include "site.h"

/*
#define WRITEMAP
*/



double 
distance (double from[2], double to[2])
{
static int first=1;
double G_distance();
  
    if(first){
        first=0;
        G_begin_distance_calculations();
    }
    return  G_distance(from[0], from[1], to[0], to[1]);
}

double 
calc_min_territory (double e_ing, double n_ing, double thresh, double rad_incr, FCELL *ibuf, FCELL *dbuf, struct Cell_head *w, int shh)
{
double sum, radius, ns_dist, ew_dist, sitexy[2], cellxy[2];
int i, row, col, ncells, totcells;
FCELL *buf;
int boxrow1, boxrow2, boxcol1, boxcol2;
static int cnt=1;

    if(!shh)
	fprintf(stderr,"Calculating areas for site %d...\n",cnt++);

    sitexy[0] = e_ing;
    sitexy[1] = n_ing;
    totcells = w->rows * w->cols;
    for(buf = dbuf, i=0; i<totcells; i++, buf++) *buf = -1; 

    boxrow1 = G_northing_to_row(n_ing, w)-5;
    boxcol1 = G_easting_to_col(e_ing, w)-5;
    boxrow2 = boxrow1+10;
    boxcol2 = boxcol1+10;

    if(rad_incr == 0.0){
    /* rad_incr set to shortest distance between cells */
	cellxy[0] = sitexy[0]+w->ew_res;
	cellxy[1] = sitexy[1];
	ew_dist = distance(sitexy, cellxy);

	cellxy[0] = sitexy[0];
	cellxy[1] = sitexy[1]+w->ns_res;
	ns_dist = distance(sitexy, cellxy);

	rad_incr = (ew_dist < ns_dist? ew_dist: ns_dist);
    }

    ncells = 0;
    for(radius = rad_incr; ncells < totcells; radius += rad_incr){
        
	sum = 0.0;
	ncells = 0;
	for (row = boxrow1; row < boxrow2 && row < w->rows; row++) {
            if(row < 0) continue;
	    buf = dbuf+row*w->cols;
	    cellxy[1] = G_row_to_northing((double)(row+0.5),w);
	    for(col=boxcol1; col<boxcol2 && col < w->cols; col++){
		if(col < 0) continue;
		if(buf[col] == -1){
		    cellxy[0] = G_col_to_easting((double)(col+0.5),w);
		    buf[col] = distance(sitexy, cellxy);
		}
		if(buf[col] <= radius){
		    ncells++;
		    sum += ibuf[col+row*w->cols]; 
		}
	    }
	}
	if (sum >= thresh) {
#ifdef WRITEMAP
	    show_box(dbuf, radius, w, boxrow1, boxcol1, boxrow2, boxcol2);
	    write_fmap(dbuf, radius, w);
#endif
	    thresh = sum;
	    return(radius);
	}
	boxrow1--;
	boxcol1--;
	boxrow2++;
	boxcol2++;

    }
#ifdef WRITEMAP
	    show_box(dbuf, radius, w, boxrow1, boxcol1, boxrow2, boxcol2);
	    write_fmap(dbuf, radius, w);
#endif
    fprintf(stderr,"WARNING: Threshold %.4f not reached: %d cells summed\n", 
	    thresh, ncells);
    return (0.0);

}


int 
write_imap_fromf (int fd, FCELL *buf, struct Cell_head *w)
{
int row, col;
FCELL *rowbuf;
CELL *ibuf;

    
    ibuf = G_allocate_c_raster_buf();

    fprintf(stderr,"Writing ...");
    for (row = 0; row < w->rows; row++) {
        rowbuf = buf+row*w->cols;
	for (col = 0; col < w->cols; col++, rowbuf++) {
	    if(G_is_f_null_value (rowbuf))
		G_set_c_null_value (ibuf+col, 1) ;
	    else  
		ibuf[col] = *rowbuf;
	}
	G_percent(row, w->rows - 1, 10);
	G_put_c_raster_row (fd, ibuf);
    }
    fprintf(stderr,"\n");

    return 0;
}


#ifdef WRITEMAP

int 
write_fmap (FCELL *buf, double radius, struct Cell_head *w)
{
int row, col, fd;
char name[80];
static int cnt=1;
FCELL *tbuf, *rowbuf;

    tbuf = G_allocate_f_raster_buf();
    
    sprintf(name,"junk.dist%02d",cnt++);
    fd = G_open_raster_new(name, FCELL_TYPE);

    fprintf(stderr,"Writing %s...", name);
    for (row = 0; row < w->rows; row++) {
        rowbuf = buf+row*w->cols;
	G_percent(row, w->rows - 1, 10);
	for(col=0; col < w->cols; col++){
	    if(rowbuf[col] <= radius && rowbuf[col] != -1) 
		tbuf[col] = rowbuf[col];
	    else 
		G_set_f_null_value (tbuf+col, 1) ;
	}
	G_put_f_raster_row (fd, tbuf);
    }
    fprintf(stderr,"\n");
    G_close_cell(fd);
    free(tbuf);

}

int 
show_box (FCELL *dbuf, double radius, struct Cell_head *w, int boxrow1, int boxcol1, int boxrow2, int boxcol2)
{
int row, col;
FCELL *rowbuf;

    for (row = boxrow1; row < boxrow2 && row < w->rows; row++) {
	if(row < 0) continue;
	rowbuf = dbuf+row*w->cols;
	for(col=boxcol1; col<boxcol2 && col < w->cols; col++){
	    if(col < 0) continue;
	    if(rowbuf[col] > radius){
		rowbuf[col] = 0.0;
	    }
	}
    }

}


#endif

/* rfield is the dbl_att site field containing resource needs 
   ifield, if non-zero, is the distance to increment that site's
       radius for each iteration ( so you can make some sites more
       aggressive than others ) 
   Neither are checked if present in sites file here - check before calling!
*/

int 
calc_simultaneous_territory (Site **sites, int nsites, int rfield, int ifield, FCELL *ibuf, FCELL *dbuf, struct Cell_head *w, int quiet)
{
double radius, ns_dist, ew_dist, dist, sitexy[2], cellxy[2];
double *sum, *rad_incr;
int i, s, iter, row, col, ncells, totcells;
FCELL *buf;
Site *cursite;
int done=0, prev_done;

    totcells = w->rows * w->cols;
    G_set_f_null_value (dbuf, totcells) ; /* unclaimed */

    if(NULL == (sum = (double *)malloc(nsites*sizeof(double)))){
	fprintf(stderr, "ERROR: out of memory\n") ;
	exit(0);
    }
    if(NULL == (rad_incr = (double *)malloc(nsites*sizeof(double)))){
	fprintf(stderr, "ERROR: out of memory\n") ;
	exit(0);
    }

    if(ifield == 0){
	/* rad_incr set to shortest distance between cells (center of map) */
	sitexy[0] = G_col_to_easting((double)((w->cols/2.)+0.5),w);
	sitexy[1] = G_row_to_northing((double)((w->rows/2.)+0.5),w);
	cellxy[0] = sitexy[0] + w->ew_res;
	cellxy[1] = sitexy[1];
	ew_dist = distance(sitexy, cellxy);

	cellxy[0] = sitexy[0];
	cellxy[1] = sitexy[1]+w->ns_res;
	ns_dist = distance(sitexy, cellxy);

	for(i=0;i<nsites;i++) 
	    rad_incr[i] = (ew_dist < ns_dist? ew_dist: ns_dist);
    }
    else{
	for(i=0;i<nsites;i++) 
	    rad_incr[i] = sites[i]->dbl_att[ifield-1];
    }

    for(i=0;i<nsites;i++) sum[i] = 0.0;
    for(iter = 1, ncells = 0; ncells < totcells; iter++){

	if(!quiet){
	    system("date");
	    fprintf(stderr,"%.2f %% of cells allocated.\n", 
		100.*ncells/totcells);
	    fprintf(stderr,"Grow radius = %.2f ", iter*rad_incr[0]);
	    if(ifield)
		for(i=1;i<nsites;i++) 
		    fprintf(stderr,"%.2f ", iter*rad_incr[i]);
	    fprintf(stderr,"\n");
	}

	for (row = 0; row < w->rows; row++) {
	    buf = dbuf+row*w->cols;
	    cellxy[1] = G_row_to_northing((double)(row+0.5),w);
	    for(col=0; col < w->cols; col++){

		if(G_is_f_null_value (buf+col)){ /* not claimed */
		    cellxy[0] = G_col_to_easting((double)(col+0.5),w);
		    for (s=0; s<nsites; s++){
			cursite=sites[s];
			if (sum[s] < cursite->dbl_att[rfield-1]){
			    sitexy[0] = G_adjust_easting (cursite->east, w);
			    sitexy[1] = cursite->north;
			    radius = iter * rad_incr[s];
			    dist = distance(sitexy, cellxy);
			    if(dist <= radius){
				/* assuming CELL category */
				buf[col] = cursite->ccat;
				ncells++;
				sum[s] += ibuf[col+row*w->cols]; 
			    }
			}
		    }
		}
	    }
	}
	{ 
	    prev_done = done;
	    done=0;
	    for(i=0;i<nsites;i++){
	        if (sum[i] >= sites[i]->dbl_att[rfield-1])
		   done++;
	    }
	    if(!quiet && done != prev_done)
		fprintf(stderr,"%d sites COMPLETE\n", done);
	    if (done == nsites) {
		if(!quiet)
		    fprintf(stderr,"\n");
		free(sum); 
		return (1);
	    }
	}
    }

    fprintf(stderr, "\nWARNING: %d thresholds not reached\n", nsites-done) ;
    free(sum);
    return (0);
}
