static char rcsid[] = "$Header$";
/*
$Log$
Revision 1.1  1999-12-29 15:12:12  markus
Initial revision

 * Revision 1.2  1991/06/25  04:29:21  paul
 * *** empty log message ***
 *
 * Revision 1.1  1991/06/25  03:31:35  paul
 * Initial revision
 *
 * Revision 1.2  1991/06/12  01:08:57  grass
 * *** empty log message ***
 *
 * Revision 1.1  1991/05/29  03:50:43  paul
 * Initial revision
 *
*/

#include <stdio.h>
#include <ctype.h>
#define MAXLINE	300
#define HAWAII 0

#include "digit.h"
#include "gis.h"

int cat_cnt;
int	new_format ;
int	gef_format ;

imp_tig(Map,dig_name, cfclist, tig_file1,tig_file2,dig_att,scstig,verbose)
struct Map_info *Map;
char **cfclist;
FILE *tig_file1, *tig_file2;
char *dig_name ;
FILE *dig_att;
int scstig,verbose;

{
	char buff[328], buff_start[4], out_buff[328] ;
	char tmpbuf[256], selcode[75], cfcc[12], number[16], buf0[11],buf1[11], buf2[11], buf3[11], buf4[11];
	char  *buf, *cat_label;
	unsigned long recnum, recnum1, recnum2;
	float x1, y1, x2, y2, lat, lon, lat1, lon1, lat2, lon2;
	double orig_scale,E,N,S,W, N1,S1,W1,E1,N2,S2,W2,E2, map_thresh ;
	int i, x, cat_num, level=0, n_coors;
	int num_lines, num_lines_skip, num_labels, is_area;
	FILE *temp;
	int zone, cfcmatch;
	struct Categories Cats;
	struct line_pnts *Points;

	zone = Map->head.plani_zone;
	N = E = W = S = 0.0;
	num_lines = 0 ;
	num_lines_skip = 0;
	num_labels = 0 ;
	is_area = 0;

	Points = Vect_new_line_struct ();
	if (G_read_vector_cats(dig_name,G_mapset(),&Cats) < 0)
		G_fatal_error("Can't read dig_cats");
		
	temp = tig_file1;
	if(verbose) fprintf(stderr,"\n\n\tLines matched, processed :     0,      0 ");

	if (scstig == 1) /* skip FIPS code for scstig files */
		fgets(buff, MAXLINE, tig_file1); 

	while((fgets(buff, MAXLINE, tig_file1) != NULL)) {
		/* fprintf(stderr,"%s",buff);  */
		num_lines_skip++;
		if( ( (num_lines_skip % 50) == 0) && verbose)
			fprintf(stderr,"\b\b\b\b\b\b\b%6d ",num_lines_skip);
		level = 1;
		if (level == 1 ){
			buf = buff;

			if (scstig == 1)
				G_strncpy(buf0, buf, 10);  /* record number */
			else
				G_strncpy(buf0, buf+5, 10);  /* record number */
			/* narrow selection of records to convert,
				by using HEAD.OTHER INFO 1st 3 chars */

			if (strncmp(cfclist[0], "BOU", 3) == 0){
				if ((*(buf+15) != 49) && (scstig != 1)) {
					continue;
				}
				if ((*(buf+10) != 49) && (scstig == 1)) {
					continue;
				}
				is_area = 1;   /* need to make an area */
				G_strncpy(cfcc,"BOU\0",3);
			}else{
				/* get cfcc code from TIGER file */
				if (scstig == 1) /* use short scs tiger file */
					G_strncpy(cfcc,buf+11,3);  
				else
					G_strncpy(cfcc,buf+55,3); 
				/* check here for matches or continue */
				cfcmatch = 0;
				for (x=0;cfclist[x] != NULL;x++){
					if ((strncmp(cfclist[x], cfcc,
						strlen(cfclist[x])) == 0)) 
						cfcmatch = 1;
				}
				if (cfcmatch != 1) continue;
			}
		/*	name_to_number(cat_file,cfcc,&cat_num);*/
			for (x=1;x<=Cats.num;x++){
				cat_label = G_get_cat(x,&Cats);
				G_strncpy(tmpbuf,cat_label,3); 
				if ((strncmp(cfcc, tmpbuf, strlen(cfcc)) != 0))  continue;
				cat_num = x;
			}
			if (cat_num < 0) {
				fprintf(stderr,"Error: Category code creation\n");
				return(-1);
			}

			num_lines++;
			/* if((num_lines % 50) == 0) */
			if(verbose) fprintf(stderr,"\b\b\b\b\b\b\b\b\b\b\b\b\b\b%5d, %6d ",num_lines,num_lines_skip);

			/* read lat long and skip sign */
			if (scstig == 1){ /* use short scs tiger file */
				G_strncpy(buf1,buf+15,9);    /* long from */
				G_strncpy(buf2,buf+25,8);    /* lat from */
				G_strncpy(buf3,buf+34,9);    /* long to */
				G_strncpy(buf4,buf+44,8);    /* lat to */
			}else{  /* use complete Census tiger file */
				G_strncpy(buf1,buf+191,9);    /* long from */
				G_strncpy(buf2,buf+201,8);    /* lat from */
				G_strncpy(buf3,buf+210,9);    /* long to */
				G_strncpy(buf4,buf+220,8);    /* lat to */
			}
			sscanf(buf0, "%lu", &recnum);
			sscanf(buf1, "%f", &x1);
			sscanf(buf2, "%f", &y1);
			sscanf(buf3, "%f", &x2);
			sscanf(buf4, "%f", &y2);

			lat1 = y1 / 1000000.0 * 3600.0;
			lon1 = x1 / 1000000.0 * 3600.0;
			lat2 = y2 / 1000000.0 * 3600.0;
			lon2 = x2 / 1000000.0 * 3600.0;
			lat1 = (lat1 >= 0) ? lat1 : -lat1;
			lon1 = (lon1 >= 0) ? lon1 : -lon1;
			lat2 = (lat2 >= 0) ? lat2 : -lat2;
			lon2 = (lon2 >= 0) ? lon2 : -lon2;
	/*fprintf(stderr,"\n%f %f %f %f \n",lat1,lon1,lat2,lon2); */
			if (HAWAII)
				CC_u2ll_spheroid("international");
			else
				CC_u2ll_spheroid("clark66");
			CC_ll2u(lat1,lon1,&E1,&N1,&zone);
			CC_ll2u(lat2,lon2,&E2,&N2,&zone);
	/*fprintf(stderr,"\n%f %f %f %f %d\n",lat1,lon1,N1,E1,zone); */


			/* write out SHAPE records to dig file between end points */
			write_shape(Map,Points,recnum, dig_att,
			    tig_file2,N1,E1,N2,E2,cat_num,is_area);

			/* keep max and min extents to fill in header */
			if(N == 0.0)  N = N1;
			if(S == 0.0)  S = N1;
			if(E == 0.0)  E = E1;
			if(W == 0.0)  W = E1;

			if(N1 > N) N = N1;
			if(N1 < S) S = N1;
			if(E1 > E) E = E1;
			if(E1 < W) W = E1;

			if(N2 > N) N = N2;
			if(N2 < S) S = N2;
			if(E2 > E) E = E2;
			if(E2 < W) W = E2;

		}
	}
	Map->head.N = N;
	Map->head.S = S;
	Map->head.E = E;
	Map->head.W = W;
 /*fprintf(stderr,"N= %13.2lf, S= %13.2lf,\
	 W= %13.2lf, E= %13.2lf\n",Map->head.N,S,Map->head.W,E);*/
	if (is_area == 1){
		sprintf(out_buff,"A %13.2lf %13.2lf     %d\n",(E-W)/2.0+W,(N-S)/2.0+S,cat_num);
		fputs(out_buff, dig_att);
	}
	if(verbose) fprintf(stderr,"\b\b\b\b\b\b\b%6d ",num_lines_skip);
	Vect_destroy_line_struct(Points);
	return(0);
}


write_shape(Map,Points,recnum, dig_att, tig_file2, N1, E1, N2, E2, cat_num, is_area)
struct Map_info *Map;
struct line_points *Points;
FILE *dig_att, *tig_file2;
double N1, E1, N2, E2;
int cat_num, is_area;
unsigned long recnum;

{
	char buf[350], buf1[20], buf2[40], out_buff[80], *bufp, buflon[15], buflat[15];
	unsigned long recnum2, old_pos, new_pos;
	double N, E;
	float lat, lon;
	int x,y,z,match;
	double *xarray ;
	double *yarray ;
	double *xptr, *yptr ;
	int i ;
	int n_points ;
	int type ;
	int alloc_points ;
	int end_of_file ;
	int zone;

	zone = Map->head.plani_zone;


	end_of_file = 0 ;
	alloc_points     = 1000 ;
	xarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	yarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;

	n_points = 0 ;
	xptr = xarray ;
	yptr = yarray ;

	bufp = buf;
	buf1[0] = '\0';


	/* generate coords.  write out line prefix and 1st pnt, value of 2
		 for # of points is as good as any */
	if (is_area)
		type = AREA;
	else
		type = LINE;
	*xptr = E1;
	*yptr = N1;
	xptr++;
	yptr++;
	n_points++;

/* old_pos = ftell(tig_file2); */
	x = y = 1;
	match = 0;
	while(fgets(buf, MAXLINE, tig_file2) != NULL){
		bufp = buf;
		G_strncpy(buf1, bufp+5, 10);
		sscanf(buf1, "%lu", &recnum2);
		if (recnum2 > recnum)  break;
		if (recnum == recnum2){
			match = 1;
	/*		fprintf(stderr,"match recnum =%lu recnum2 =%lu\n",
				recnum,recnum2);     */
/*			old_pos = ftell(tig_file2); */
			bufp = buf + 19;

			/* loop through up 10 set of coords for shape rec */

			for(x=1;x<11;x++,y++,bufp += 19){
				G_strncpy(buflon, bufp, 9);
				G_strncpy(buflat, bufp + 10, 8);
				sscanf(buflon, "%f", &lon);
				sscanf(buflat, "%f", &lat);
				/*fprintf(stderr,"\n%f %f %s %s\n%s\n",
					lon,lat,buflon, buflat, buf); */

				/* if there are values in shape file write
					it out to dig or quit now */
				if( lon != 0.0 ){
					lon = (lon >= 0 ) ? lon : -lon;
					lat = (lat >= 0 ) ? lat : -lat;
					lon = lon / 1000000. * 3600.0;
					lat = lat / 1000000. * 3600.0;
					if (HAWAII)
						CC_u2ll_spheroid("international");
					else
						CC_u2ll_spheroid("clark66");
					CC_ll2u(lat,lon,&E,&N, &zone);
					/* fprintf(stderr,"%13.2lf%13.2lf\n"
						,N,E);*/
					sprintf(buf2,"%13.2lf%13.2lf\n",N,E);
					*xptr = E;
					*yptr = N;
					n_points++ ;
					xptr++;
					yptr++ ;

			if (n_points + 1  >= alloc_points)
			{
				xarray = (double *)dig_frealloc((char *)xarray, alloc_points + 1000, sizeof(double), alloc_points);
				yarray = (double *)dig_frealloc((char *)yarray, alloc_points + 1000, sizeof(double), alloc_points);
				alloc_points = n_points + 1000 ;
				xptr = xarray + n_points ;
				yptr = yarray + n_points ;
			}
					/* use the 1st shape coord for dig_att
						to elim dupes on endpts or
						use midpt if no shape coord */
					if((y == 1) && (is_area != 1)){
						sprintf(out_buff, "L  %13.2lf %13.2lf     %5d\n", E,N,cat_num);
						fputs(out_buff,dig_att);
					}
				} /* no more shape coords */
				else break;
			} /* break out of FOR LOOP for shape coords */
			if (lon == 0.0 ) break;
		} /* ENDIF for recnum == recnum2 */
	} /* END WHILE for fgets */
	/* write out final point on segment from data file */
	sprintf(buf2,"%13.2lf%13.2lf\n",N2,E2);
	/*fputs(buf2,dig);*/
	*xptr = E2;
	*yptr = N2;
	n_points++;
	xptr++;
	yptr++;
	Vect_copy_xy_to_pnts(Points, xarray, yarray, n_points);
	if(Vect_write_line(Map, type, Points) <= 0)
		G_fatal_error("Can't write line");
	if ((match == 0) && (is_area != 1)){
		/* make attr. record */
		E = (E2 - E1)/2 + E1;
		N = (N2 - N1)/2 + N1;
		sprintf(out_buff,"L  %13.2lf %13.2lf     %5d\n",E,N,cat_num);
		fputs(out_buff,dig_att);
	}
/*	rewind(tig_file2);
	fseek(tig_file2, old_pos, 0);
 */
	fseek(tig_file2, -209, 1);
	free(xarray);
	free(yarray);
}  /* end of funct */
