#ident "  @(#)geo_supp.c    1.0  3/18/91  "
/*
**  created by R.L.Glenn
**  USDA Soil Conservation Service, CGIS Division
*
*   set of support function for geo
*/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "geo.h"

 /*  show the user results and/or put to a file */
Write_results(end)
int end;
{
      char line[80], AZONE[6];

      if (strncmp(proj_name,"stp",3) != 0 && ZONE == 0) sprintf(AZONE,"none");
      else if (strncmp(proj_name,"stp",3) == 0) sprintf(AZONE,"%4d",COzone);
      else sprintf(AZONE," %2d ",ZONE);

      if (!end)
        {
        if (output_typ == 1)
	  {                                    /*  put out the heading */
          fprintf(stderr,"\n\n    Longitude      Latitude         Easting(x)          Northing(y)    Zone\n");
          fprintf(stderr,"   -----------    ----------       ------------        ------------   ------\n");
	  sprintf(line,"%+2d%3d%8.4f  %+3d%3d%8.4f      %10.2f         %10.2f     %s\0",IDEG,IMIN,XSEC,JDEG,JMIN,YSEC,EAS,NOR,AZONE);
	  fprintf(stderr,"\n%s\n",line);
	  fprintf(stderr,"\n                                         To continue    -Hit any key- ");
	  gets(answer);
	  }
        else
	  {
	  sprintf(line,"%+2d%3d%8.4f   %+3d%3d%8.4f      %10.2f         %10.2f     %d\n",IDEG,IMIN,XSEC,JDEG,JMIN,YSEC,EAS,NOR,ZONE);
	  fputs(line, Out_file);
	  }
        }

      if (end)
	{
	if (output_typ == 2)
          {
          fclose(Out_file);
	  sprintf(line,"   %6d coordinates processed, to continue    -Hit any key- ",rec_cnt);
	  fprintf(stderr,"\n%s\n",line);
	  gets(answer);
  	  }
        }
}

get_mem(r_w)
int r_w;
{
	 int ier;
	 FILE *Mem_file;

         sprintf(in_file,"%s/%s/.tmp/geo_mem\0", G_location_path(), G_mapset());
	 if (r_w == 0)
	    {
	    if ( (Mem_file = fopen(in_file, "w")) == NULL) return(0);
            else
	       {
	       COzone = 0;
	       proj_name[0] = '\0';
	       ellps_name[0] = '\0';
	       LONCEN = -96.0;
	       LATPAR = 23.0;
	       sprintf(buff,"0	No_County	XX	0	-96	23	None	None\0");
	       fputs(buff,Mem_file);
	       fclose(Mem_file);
	       return(1);
	       }
	    }
         if (r_w == 1)
            {
	    if ( (Mem_file = fopen(in_file, "r")) == NULL) return(0);
            else
	       {
	       rewind(Mem_file);
               if (fgets(buff,80,Mem_file) == NULL) return(0);
 	       sscanf(buff,"%d%d%s%s%d%lf%lf%s%s",
	             &SFIPS,&CFIPS,COname,STabbr,&COzone,
		     &LONCEN,&LATPAR,proj_name,ellps_name);
               fclose(Mem_file);
	       }
            }
         if (r_w == 2)
            {
	    if ( (Mem_file = fopen(in_file, "w")) == NULL) return(0);
            else
	       {
 	       sprintf(buff,"%d\t%d\t%s\t%s\t%d\t%lf\t%lf\t%s\t%s",
				SFIPS,CFIPS,COname,STabbr,COzone,
				LONCEN,LATPAR,proj_name,ellps_name);
	       fputs(buff,Mem_file);
	       fclose(Mem_file);
	       }
            }
	 return(1);
}

get_file(method)
int method;
{
	 G_clear_screen();
         fprintf(stderr," Enter File Name > ");
	 gets(answer);
         if (method == 1)
            {
	    sprintf(in_file,"%s",answer);
	    if ( (In_file = fopen(in_file, "r")) == NULL)
	       {
	       fprintf(stderr,"\n File name could not be found/created\n");
	       sleep(2);
	       input_typ = 0;
	       }
            }
         if (method == 2)
            {
	    sprintf(out_file,"%s",answer);
	    if ( (Out_file = fopen(out_file, "w")) == NULL)
	       {
	       fprintf(stderr,"\n File name could not be found/created\n");
	       sleep(2);
	       output_typ = 0;
	       }
            }
}
