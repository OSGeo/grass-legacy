/* @(#) 1.11 6/27/90 /usr/grass3.1/src.scs/scspsu/s.do_lines.c */
/* @(#) 1.20 4/23/91 /usr/grass4.0/src.contrib/SCS/psu/s.do_lines.c */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Vect.h"
#include "gis.h"

#define MAXLINE 1000


int cur_category;
struct line_pnts *Points;

do_lines( struct Map_info *Map , struct Map_info *Out_Map ,
	FILE *psu_att,FILE *psu_data, struct Categories *cats,
	double xdist,double ydist)
{
    int at_line ;
    double *yarr, ypts[10], *yp , psu_x[3], psu_y[3], tmpx[3], tmpy[3];
    double *xarr, xpts[10], *xp ;
    int n_vert ;
    int max_lines ;
    int pkgs ;
    int a, b, psu_acres, psu_pts,x;
    char *catlistptr;
    double angle, xoff, yoff, xtmp, ytmp;
    char psu_buf[1000], tmpbuf[100], tmpbuf2[1000];
    struct  line_pnts  *Pnts ;  
    CELL cat_num, max_cat;
    char *cat_label, psu_num[10], *dummy;


    pkgs = 0 ;
    max_cat = cats->num;
    max_lines = V2_num_lines(Map) ;

/*    fprintf (stdout, "Step 3: Creating Line information...   ") ;*/
    for(at_line=1; at_line <= max_lines; at_line++)
    {
	if (Map->Line[at_line].type == DOT)
	    continue;

	cur_category = V2_line_att( Map, at_line) ;

	if ( ! cur_category)
		continue ;
	cat_label = G_get_cat( cur_category, cats);
	G_strip(cat_label);

		/* need to look at psu file for data */
	while (((dummy = fgets(psu_buf, MAXLINE, psu_data)) != NULL)){
		G_strncpy(psu_num, psu_buf+6, 7);
		b=strcmp(psu_num , cat_label);
			/*fprintf (stdout,"psunum *%s* , catlabel *%s*, cate %d\n", psu_num, cat_label,cur_category);  */
		if (b == 0)
			break;
	}
	if (dummy == NULL){
		fprintf (stdout,"catlabel *%s* not found for cat %d\n", cat_label,cur_category); 
		exit(-1);
	}
	rewind (psu_data);
	G_strncpy(tmpbuf, psu_buf+19, 3);
	sscanf(tmpbuf, "%d", &psu_acres);
/*	fprintf (stdout,"acres %d \n", psu_acres);    */
	psu_pts = 3;
	switch (psu_acres){
		case 640:
			xdist = 1609.344;
			ydist = 1609.344;
			break;
		case 160:
			xdist = 804.672;
			ydist = 804.672;
			break;
		case 40:
			psu_pts = 2;
			xdist = 402.336;
			ydist = 402.336;
			break;
		default:
			if( xdist == 0.0 && ydist == 0.0 ){
				fprintf (stdout,"No PSU size entered!\n");
				exit(-1);
			}
	}

		/* read dig line from dig file */
	if (0 > V1_read_line (Map, Points, Map->Line[at_line].offset)) 
		return(-1) ;

	pkgs++ ;

/*  break out x and y  */
	n_vert = Points->n_points ;
	xarr = Points->x ;
	yarr = Points->y ;

   n_vert = 2;
	for(a=0;a < n_vert;a++,xarr++,yarr++){
	/*	fprintf (stdout," Point    %f, %f \n",*xarr,*yarr); */
		xpts[a] = *xarr;
		ypts[a] = *yarr;
	}
	angle = atan2((ypts[1] - ypts[0]) , (xpts[1] - xpts[0]));
	xoff = xpts[0]; yoff = ypts[0];
	xpts[0] = ypts[0] = xpts[1] = 0.0;
	xpts[2] = xpts[3] = xdist;
	ypts[1] = ypts[2] = ydist;
	ypts[3] = xpts[4] = ypts[4] = 0.0;
	xpts[0] = xoff;	ypts[0] = yoff;	
	rotate(&xpts[1], &ypts[1], angle);
	xpts[1]+=xoff;
	ypts[1]+=yoff;	
	rotate(&xpts[2], &ypts[2], angle);
	xpts[2]+=xoff;
	ypts[2]+=yoff;	
	rotate(&xpts[3], &ypts[3], angle);
	xpts[3]+=xoff;
	ypts[3]+=yoff;	
/*
	xtmp = xpts[1]*sin(angle) + ypts[1]*cos(angle) + xoff;
	ytmp = ypts[1]*sin(angle) - xpts[1]*cos(angle) + yoff;
	xpts[1] = xtmp; ypts[1] = ytmp;
	xtmp = xpts[2]*sin(angle) + ypts[2]*cos(angle) + xoff;
	ytmp = ypts[2]*sin(angle) - xpts[2]*cos(angle) + yoff;
	xpts[2] = xtmp; ypts[2] = ytmp;
	xtmp = xpts[3]*sin(angle) + ypts[3]*cos(angle) + xoff;
	ytmp = ypts[3]*sin(angle) - xpts[3]*cos(angle) + yoff;
	xpts[3] = xtmp; ypts[3] = ytmp;
*/
	xpts[4] = xoff; ypts[4] = yoff;
	n_vert = 5;

	xp = xpts;
	yp = ypts;
	for(a=0;a < n_vert;a++,xp++,yp++){
	/*	fprintf (stdout," Point %d   %f, %f \n",a,*xp,*yp);*/
	}
		/* set pointers back to prep for line write */
	xp = xpts;
	yp = ypts;
	Vect_copy_xy_to_pnts(Points, xp, yp, n_vert);
	Vect_write_line(Out_Map, AREA, Points);
		/* set middle of box for att location */
	xpts[5] =  (xpts[2] - xpts[0]) / 2. + xpts[0];
	ypts[5] =  (ypts[2] - ypts[0]) / 2. + ypts[0];
	fprintf( psu_att, "A  %12.2f  %12.2f        %d\n", xpts[5], ypts[5],cur_category);
/* G_set_cat((CELL) psu_num, psu_cat_buf, cats);*/

		/* need to take care of psu points now */
		/* get psu points from file and convert to meters */	
	for (a=0;a<psu_pts;a++){
		G_strncpy(tmpbuf, psu_buf+23+10*a, 4);
		sscanf(tmpbuf, "%d", &b);
		psu_y[a] = (double) b * 0.3048;
		G_strncpy(tmpbuf, psu_buf+28+10*a, 4);
		sscanf(tmpbuf, "%d", &b);
		psu_x[a] = (double) b * 0.3048;
		rotate(&psu_x[a], &psu_y[a], angle);
		psu_x[a]+=xoff;
		psu_y[a]+=yoff;
		/*fprintf (stdout,"points %f, %f\n", psu_y[a],psu_x[a]);*/
			/* need two points for point type line */
		tmpx[0] = tmpx[1] = psu_x[a];
		tmpy[0] = tmpy[1] = psu_y[a];
		xp = tmpx;
		yp = tmpy;
	        Vect_copy_xy_to_pnts(Points, xp, yp, 2);
		Vect_write_line(Out_Map, DOT, Points);
			/* need to label points with cat num from subj file */
		catlistptr = cats->labels;
		catlistptr++;
		sprintf(tmpbuf, "%s%d\0", cat_label, a+1);
		for (x=1;x , cats->num ; x++,catlistptr++){
			G_strcpy( tmpbuf2, catlistptr);
			if (strncmp( tmpbuf, tmpbuf2, strlen(tmpbuf)) == 0){
				/*fprintf (stdout,":%s:  :%s:\n",tmpbuf,  tmpbuf2);*/
				break;
			}
		}
		max_cat++;
		fprintf( psu_att, "P  %12.2f  %12.2f        %d\n", tmpx[0], tmpy[0], x);
/*   don't add any new cats. All in from psusubj.
		sprintf(tmpbuf, "%s%d\0", cat_label, a+1);
		G_set_cat((CELL) max_cat, tmpbuf, cats);
*/
	}
    } /* end of lines in dig */

    return(pkgs) ;
}

rotate(x,y,angle)
	double *x, *y, angle;
{
	double xtmp,ytmp;

	xtmp = *x * sin(angle) + *y * cos(angle);
	ytmp = *y * sin(angle) - *x * cos(angle);
	*x = xtmp;
	*y = ytmp;
}
