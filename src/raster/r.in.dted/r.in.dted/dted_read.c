#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "dtedgis.h"

/*
 * $Id$
 */

/* for signed bytes:
 * num must be >0 and <sizeof(int) 
 * inc must have at least num bytes
*/
int sbytes_to_int(unsigned char *inc, int num)
{
int ret=0;
int i, nbit;

    for (i=0; i<num; i++){
	if(i) ret <<= 8;
	ret |= inc[i];
    }

    /* check for negative */
    nbit = 0x80 << (8*(num-1));
    if( nbit & ret )
	ret = -(ret^nbit);

    return (ret);

}

/* for unsigned bytes:
 * num must be >0 and <sizeof(int) 
 * inc must have at least num bytes
*/
int 
ubytes_to_int(unsigned char *inc, int num)
{
int ret=0;
int i;

    for (i=0; i<num; i++){
	if(i) ret <<= 8;
	ret |= inc[i];
    }

    return (ret);

}

int 
dmshtodd (char *dmsh, double *dd)
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

#define BITCHECK

/* 
 *  GRASS data is at center of cell, so need to adjust region
 *  boundaries by 1/2 cell for proper overlap & accurate data
 *  location.  This causes unresolved conflict at poles -
 *  GRASS will complain of illegal region.  Most accurate solution
 *  is to drop data point at pole, skipping GRASS first 
 *  (or last, for south pole) row, which should be a single 
 *  repeated value in the dted.
*/
int get_header (FILE *infp, struct Cell_head *cellhd, int *pole_flag)
{
long dlon, dlat, nlon, nlat;
double dlond, dlatd, swlond, swlatd;
char hrec[80];
char swlat[9], swlon[9], class[4];


#ifdef BITCHECK
{
int d1,d2;

d1=0x80000000;
d2=0x80000000;

fprintf(stderr,"sizeof short = %ld\n",sizeof(short));

if(d1&d2){
fprintf(stderr,"%d & %d = TRUE\n", d1, d2);
fprintf(stderr,"-1 = %X\n", -1);
fprintf(stderr,"-2 = %X\n", -2);
fprintf(stderr,"0x80000001 = %d\n", 0x80000001);
fprintf(stderr,"0xFFFFFFFF + 1 = %d\n", 0xFFFFFFFF+1);
}
else
fprintf(stderr,"%d & %d = FALSE\n", d1, d2);

}
#endif

    *pole_flag = 0;

    fseek(infp,0L,SEEK_SET);
    if(80 != fread (hrec,sizeof(char),80,infp))
	return(-1);

    sscanf (hrec,"%*4s%8s%8s%4ld%4ld%*4c%3s%*12c%4ld%4ld",
                swlon,swlat,&dlon,&dlat,class,&nlon,&nlat);

   /* Set the SW corner values */

    if(0 > dmshtodd(swlon,&swlond)) 
	return(-1);
    if(0 > dmshtodd(swlat,&swlatd))
	return(-1);
    dlond = (double)dlon/36000.;
    dlatd = (double)dlat/36000.;

   /* Set the extremes */

    cellhd->rows = nlat; /* usually 901, 1201 or 3601 */
    cellhd->cols = nlon; /* depends on latitude */
    cellhd->proj = G_projection();
    cellhd->zone = G_zone();
    cellhd->ew_res = dlond;
    cellhd->ns_res = dlatd;
    cellhd->south = swlatd-(dlatd/2.0); 
    cellhd->north = cellhd->south + (double)nlat*dlatd;

    /* GRASS will adjust eastings for possible wrap */
    cellhd->west = swlond-(dlond/2.0); 
    cellhd->east = cellhd->west + (double)nlon*dlond;

    if (cellhd->south < -90.0){
	*pole_flag = SOUTH_POLE;
	cellhd->south += dlatd;
	cellhd->rows -= 1;
    }

    if (cellhd->north > 90.0){
	*pole_flag = NORTH_POLE;
	cellhd->north -= dlatd;
	cellhd->rows -= 1;
    }
    
    return(1);

}

int add_dted_hist (FILE *infp, struct History *hist)
{
char hdr[1024];
char hbuf[RECORD_LEN], hbuf2[RECORD_LEN], hbuf3[RECORD_LEN];


    fseek(infp,0L,SEEK_SET);
    if(748 != fread (hdr,sizeof(char),748,infp))
	return(-1);
    
    if (hist->edlinecnt >= MAXEDLINES) return(0);
   
    /* DTEDn */
    sscanf(hdr+139,"%5s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], "%s",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    /* Classification */
    strncpy(hbuf,hdr+86,27); hbuf[27] = '\0';
    sprintf(hist->edhist[hist->edlinecnt++], "%s",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    /* Edition/Version */
    sscanf(hdr+167,"%2s%1s",hbuf,hbuf2);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Edition/Version: %s/%s",hbuf,hbuf2);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    /* Specification */
    sscanf(hdr+206,"%9s%2s%4s",hbuf,hbuf2,hbuf3);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Specification %s.%s (%s)",hbuf,hbuf2,hbuf3);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+28,"%4s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Absolute Vertical Accuracy (HDR): %s meters",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+221,"%3s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Vertical Datum: %s",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+224,"%5s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Horizontal Datum: %s",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+239,"%4s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Data Compilation Date: %s",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    strncpy(hbuf,hdr+229,10); hbuf[10] = '\0';
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Digitize/Collect note: %s",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sprintf(hist->edhist[hist->edlinecnt++], 
	    "Supplemental Accuracy Information:");
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+731,"%4s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "    Absolute Horizontal: %s meters",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+735,"%4s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "    Absolute Vertical: %s meters",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+739,"%4s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "    Point-to-point Horizontal: %s meters",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    sscanf(hdr+743,"%4s",hbuf);
    sprintf(hist->edhist[hist->edlinecnt++], 
	    "    Point-to-point Vertical: %s meters",hbuf);
    if (hist->edlinecnt >= MAXEDLINES) return(0);

    return 0;
}


/* ********************************************************************
 * Returns column number read in header
*/

int 
read_record (FILE *ifp, int points, dted_d *buf)
{
unsigned char rec[8];
int i, dblock, checksum=0, row, col;



    /* read record header */
    fread (rec,sizeof(char),8,ifp);

    if(DB_RECOG != ubytes_to_int(rec,1)) /* recognition */
	return (-1);

    dblock = sbytes_to_int(rec+1,3);
    col = sbytes_to_int(rec+4,2);
    row = sbytes_to_int(rec+6,2);

    for (i=0; i<8; i++)
	checksum += (int)rec[i];

    for (i=0; i<row; i++){
	buf[i] = DTED_NULL;
    }

    for (i=row; i<points; i++){
	fread (rec,sizeof(char),2,ifp);
	checksum += (int)rec[0];
	checksum += (int)rec[1];
	buf[i] = sbytes_to_int(rec,2);
    }

    fread (rec,sizeof(char),4,ifp);
    if(checksum != sbytes_to_int(rec,4))
	return(-2);

    return(col);
}

/* for debugging */
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

/* for debugging */
int check_record (FILE *ifp, int points)
{
unsigned char rec[8];
int i, checksum=0, row;
/* read record header */

    fread (rec,sizeof(char),8,ifp);
    fprintf(stderr,"recognition: %d\n", ubytes_to_int(rec,1));
    fprintf(stderr,"DBC:         %d\n", sbytes_to_int(rec+1,3));
    fprintf(stderr,"col:         %d\n", sbytes_to_int(rec+4,2));
    fprintf(stderr,"row:         %d\n", row=sbytes_to_int(rec+6,2));

    for (i=0; i<8; i++)
	checksum += (int)rec[i];

    for (i=row; i<points; i++){
	fread (rec,sizeof(char),2,ifp);
	checksum += (int)rec[0];
	checksum += (int)rec[1];
	fprintf(stderr," %d,%d=%d",(int)rec[0],(int)rec[1],sbytes_to_int(rec,2));
    }

    fread (rec,sizeof(char),4,ifp);
    fprintf(stderr,"points: %d    chkread: %d     chkcalc: %d\n", 
	    points-row, sbytes_to_int(rec,4), checksum);

    return 0;
}
