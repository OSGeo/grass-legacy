#include <gis.h>
#include <stdio.h>
#include "dtedgis.h"

int bytes_to_int(inc,num)
char *inc;
int num;
{
int ret=0;
int i;

    for (i=0; i<num; i++){
	if(i) ret <<= 8;
	ret |= inc[i];
    }

/* for SWAP
    for (i=num; i>0; i--){
	if(i!=num) ret <<= 8;
	ret |= inc[i-1];
    }
*/

    return (ret);

}

int dmshtodd(dmsh, dd)
char *dmsh;
double *dd;
{
int d,m,s;
char h;

    if(4 != sscanf(dmsh,"%3d%2d%2d%c", &d,&m,&s,&h))
        return(-1);
    
    *dd = d + m/60. + s/3600.;
    if(h == 'W' || h == 'S')
    *dd *= -1;

    return(1);

}


/* 
 *  GRASS data is at center of cell, so need to adjust region
 *  boundaries by 1/2 cell for proper overlap & accurate data
 *  location.  This causes unresolved conflict at poles -
 *  GRASS will complain of illegal region. 
*/
get_header(infp, cellhd)
FILE *infp;
struct Cell_head *cellhd;
{
int dted_width, dted_height;
long dlon, dlat, nlon, nlat;
double dlond, dlatd, swlond, swlatd;
char hrec[80], ebuf[256];
char swlat[9], swlon[9], class[4];


    sprintf(ebuf,"Error reading DTED header\n");

    fseek(infp,0L,SEEK_SET);
    if(80 != fread (hrec,sizeof(char),80,infp))
	G_fatal_error(ebuf);

    sscanf (hrec,"%*4s%8s%8s%4ld%4ld%*4c%3s%*12c%4ld%4ld",
                swlon,swlat,&dlon,&dlat,class,&nlon,&nlat);

   /* Set the SW corner values */

    if(0 > dmshtodd(swlon,&swlond)) 
	G_fatal_error(ebuf);
    if(0 > dmshtodd(swlat,&swlatd))
	G_fatal_error(ebuf);
    dlond = (double)dlon/36000.;
    dlatd = (double)dlat/36000.;

   /* Set the extremes */

    cellhd->rows = nlat; /* usually 1201 or 3601 */
    cellhd->cols = nlon; /* depends on latitude */
    cellhd->proj = G_projection();
    cellhd->zone = G_zone();
    cellhd->ew_res = dlond;
    cellhd->ns_res = dlatd;
    cellhd->south = swlatd-(dlatd/2.0); 
    cellhd->north = cellhd->south + (double)nlat*dlatd;

    /* TODO: adjust easting for possible wrap */
    cellhd->west = swlond-(dlond/2.0); 
    cellhd->east = cellhd->west + (double)nlon*dlond;;


}

/* for debugging */
check_record(ifp, points)
FILE * ifp;
int points;
{
char rec[8];
int i, blockcnt, eastcnt, northcnt, checksum=0, row;
/* read record header */

    fread (rec,sizeof(char),8,ifp);
    fprintf(stderr,"recognition: %d\n", bytes_to_int(rec,1));
    fprintf(stderr,"DBC:         %d\n", bytes_to_int(rec+1,3));
    fprintf(stderr,"col:         %d\n", bytes_to_int(rec+4,2));
    fprintf(stderr,"row:         %d\n", row=bytes_to_int(rec+6,2));

    for (i=0; i<8; i++)
	checksum += (int)rec[i];

    for (i=row; i<points; i++){
	fread (rec,sizeof(char),2,ifp);
	checksum += (int)rec[0];
	checksum += (int)rec[1];
	fprintf(stderr," %d,%d=%d",(int)rec[0],(int)rec[1],bytes_to_int(rec,2));
    }

    fread (rec,sizeof(char),4,ifp);
    fprintf(stderr,"points: %d    chkread: %d     chkcalc: %d\n", 
	    points-row, bytes_to_int(rec,4), checksum);

}

/* ********************************************************************
 * Returns column number read in header
*/

int read_record(ifp, points, buf)
FILE * ifp;
int points;
dted_d *buf;
{
char rec[8];
int i, dblock, checksum=0, row, col;


    /* read record header */
    fread (rec,sizeof(char),8,ifp);

    if(DBLOCK_REC != bytes_to_int(rec,1)) /* recognition */
	return (-1);

    dblock = bytes_to_int(rec+1,3);
    col = bytes_to_int(rec+4,2);
    row = bytes_to_int(rec+6,2);

    for (i=0; i<8; i++)
	checksum += (int)rec[i];

    for (i=0; i<row; i++){
	buf[i] = DTED_NULL;
    }

    for (i=row; i<points; i++){
	fread (rec,sizeof(char),2,ifp);
	checksum += (int)rec[0];
	checksum += (int)rec[1];
	buf[i] = bytes_to_int(rec,2);
    }

    fread (rec,sizeof(char),4,ifp);
    if(checksum != bytes_to_int(rec,4))
	return(-2);

    return(col);
}

int dted_zone_compute(double latitude,long *zone)
{

   if (fabs(latitude) >= 0.E0 && fabs(latitude) < 50.E0 )
      *zone = 1;
   else if (fabs(latitude) >= 50.E0 && fabs(latitude) < 70.E0 )
      *zone = 2;
   else if (fabs(latitude) >= 70.E0 && fabs(latitude) < 75.E0 )
      *zone = 3;
   else if (fabs(latitude) >= 75.E0 && fabs(latitude) < 80.E0 )
      *zone = 4;
   else if (fabs(latitude) >= 80.E0 && fabs(latitude) <= 90.E0 )
      *zone = 6;      /* This is to accomidate 18 sec spacing */

   return 1;
}

