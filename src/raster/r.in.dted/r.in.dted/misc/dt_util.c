#include "ixvt.h"
#include <stdlib.h>
#include <math.h>
#include "typedef.h"
#include "extent.h"

extern short   dted_zone_compute (double, long *);
extern short   dted_cdrom_extent (FILE_SPEC *fs, EXTENT *);
extern short   dted_cdrom_dlat   (FILE_SPEC *fs, long *, long *);
extern short   dted_cell_extent  (FILE *fp, EXTENT *);
extern FILE    *dtFileOpenOncdir (FILE_SPEC *fs);
extern FILE    *dtFileOpenData   (FILE_SPEC *, char *, char *, long);
extern long    pdmstodd          (double, double *);
static char    *get_input_line   (char *s, int n, FILE *fp);

short dted_zone_compute(double latitude,long *zone)
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

   return TRUE;
}

short dted_cdrom_extent(FILE_SPEC *fs,EXTENT *extent)
{
    static FILE_SPEC file_spec;
    FILE *inpf;
    char *s, line[101];
    long len;
    char latc[3];
    long ew,ns;
    double lon, lat;
    long nread;

   /* Open the file */

    if ((inpf = dtFileOpenOncdir(fs)) == (FILE *)NULL)
    {
       iNote("Error opening dted onc.dir text file");
       return (FALSE);
    }

   /* Set the initial extents */

    extent->top    =  -90.E0;
    extent->bottom =   90.E0;
    extent->left   =  180.E0;
    extent->right  = -180.E0;

   /* Read the lines and compute the extents */

    while ((s = get_input_line (line,100,inpf)) != '\0')
    {
       len = strlen(line);
    /* line[len-1] = '\0';
       line[len-2] = '\0'; */

      /* Compute the latitude and check extents */

       if (strncmp(line,"         ",9) == 0)
       {
          if (line[9] == 'N')
             ns =  1;
          else
             ns = -1;
          strncpy(latc,&line[10],2);
          latc[2] = '\0';
          lat = ns * atof(latc);
          if (lat > extent->top) extent->top = lat;
          if (lat < extent->bottom) extent->bottom = lat;
       }

      /* Compute the longitude and check extents */

       else if (strncmp(line,"    ",4) == 0)
       {
          if (line[4] == 'E')
             ew =  1;
          else
             ew = -1;
          lon = ew * atof(&line[5]);
          if (lon > extent->right) extent->right = lon;
          if (lon < extent->left) extent->left = lon;
       }

    }
    fclose(inpf);
    extent->top    =  extent->top   + 1.E0;   /* allow for SW corner */
    extent->right  =  extent->right + 1.E0;

}

short dted_cdrom_dlat(FILE_SPEC *fs,long *Dlat, long *level)
{
    static FILE_SPEC file_spec;
    FILE *inpf;
    char *s, line[101];
    long len;
    char Lat[10], Lon[5], Level[2];
    char rcd[81];
    char swp[9],swl[9];
    long dlon,dlat;
    char class[4];
    long nlon,nlat;
    long nread;

   /* Open the onc.dir file */

    if ((inpf = dtFileOpenOncdir(fs)) == (FILE *)NULL)
    {
       iNote("Error opening dted onc.dir text file");
       return (FALSE);
    }

   /* Read the lines and for the first file */

    while ((s = get_input_line(line,100,inpf)) != '\0')
    {
       len = strlen(line);
    /* line[len-1] = '\0';
       line[len-2] = '\0'; */

      /* Compute the latitude */

       if (strncmp(line,"         ",9) == 0)
       {
          strncpy(Lat,&line[9],3);
          Lat[3] = '\0';
          strncpy(Level,&line[15],1);
          Level[1] = '\0';
          *level = atol(Level);
          Lat[0] = tolower(Lat[0]);
          break;
       }

      /* Compute the longitude */

       else if (strncmp(line,"    ",4) == 0)
       {
          strncpy(Lon,&line[4],4);
          Lon[4] = '\0';
          Lon[0] = tolower(Lon[0]);
       }

    }
    fclose(inpf);

   /* Open the DTED file */

    if ((inpf = dtFileOpenData(fs,Lon,Lat,(*level))) == (FILE *)NULL)
    {
       char string[100];
       sprintf(string,"Error opening DTED file %s %s.dt%ld",Lon,Lat,level);
       iNote(string);
       return (FALSE);
    }

 
   /* Read the header of the file */
 
    memset(class,0,sizeof(class));
    memset(swl,0,sizeof(swl)); memset(swp,0,sizeof(swp));
    fread (rcd,sizeof(char),80,inpf);
    sscanf (rcd,"%*4s%8s%8s%4ld%4ld%*4c%3s%*12c%4ld%4ld",
                swl,swp,&dlon,&dlat,class,&nlon,&nlat);
    if (strncmp(&rcd[207],"****",4) == 0)  /* Overflow */
       nlon =  36000.E0/(double)dlon + 1L;
    if (strncmp(&rcd[211],"****",4) == 0)  /* Overflow */
       nlat =  36000.E0/(double)dlat + 1L;
    fclose(inpf);

   /* Set the dlat */

    (*Dlat) = dlat;

    return (TRUE);

}

short dted_cell_extent(FILE *fp,EXTENT *extent)
{
    char rcd[81];
    char swp[9],swl[9];
    char ew[2], ns[2];
    long dlon,dlat;
    char class[4];
    long nlon,nlat;
    double swlon, swlat;
 
   /* Read the header of the file */
 
    memset(rcd,0,sizeof(rcd));
    memset(class,0,sizeof(class));
    memset(swl,0,sizeof(swl)); memset(swp,0,sizeof(swp));
    memset(ew,0,sizeof(ew)); memset(ns,0,sizeof(ns));
    fread (rcd,sizeof(char),80,fp);
    sscanf (rcd,"%*4s%7s%1s%7s%1s%4ld%4ld%*4c%3s%*12c%4ld%4ld",
                swl,ew,swp,ns,&dlon,&dlat,class,&nlon,&nlat);
    swl[7] = swp[7] = ew[1] = ns[1] = '\0';
    fseek(fp,0,0);

   /* Set the SW corner values */

    pdmstodd(atof(swl),&swlon);
    pdmstodd(atof(swp),&swlat);
    if (strcmp(ew,"W") == 0)
       swlon *= -1.E0;
    if (strcmp(ns,"S") == 0)
       swlat *= -1.E0;

   /* Set the extremes */

    extent->bottom = swlat;
    extent->left   = swlon;
    extent->top    = swlat + (double)(nlat-1L)*(double)dlat/36000.E0;
    extent->right  = swlon + (double)(nlon-1L)*(double)dlon/36000.E0;

}
/*****************************************************************************/
/* GET_INPUT_LINE                                                            */
/*****************************************************************************/

static char *get_input_line( char *s, int n, FILE *fp )
{
   register char *t = s;

   register int c;
   int i;
	
   if (n < 1)
      return (char *)NULL;

   i = 0;
   while (--n)
   {
      if ((c = getc(fp)) == EOF)
      {
         if (t != s)
            break;

         return (char *)NULL;
      }

      if ((c == '\n' || c == '\r' || c == '\f') && i != 0)
         break;
      else if ((c == '\n' || c == '\r' || c == '\f') && i == 0)
         continue;

      *t++ = c;
      i++;
   }
  
   *t = (char)NULL;
  
   return s;
}
