/*
*
*  THIS FUNCTION CONVERTS GRASS SITE DATA INTO MOSS IMPORT FORMAT
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/
#include <string.h>
#include "gis.h"
#include "site.h"
#include "moss.h"

int site_to_moss (
    FILE *site,     /* POINTER TO SITE_LISTS FILE */
    FILE *moss,     /* POINTER TO MOSS IMPORT FILE */
    int proj       /* PROJECTION TYPE */
)

{
    char desc[80],buf[80];
    int feature = 0;
    int ncoord = 1;
    int isle_flag = 0;
    int d=0,c=0,s=0,f=0,i;
    Site *mysite; 

    if (G_site_describe (site, &d, &c, &s, &f)!=0) 
      G_fatal_error("failed to guess format");

    mysite=G_site_new_struct(c,d,s,f);

    fprintf (stdout,"\nConverting GRASS sites to MOSS import point features\n");

    /* SEQUENTIALLY PROCESS SITES IN FILE, CONVERTING TO MOSS POINT DATA */
    while (G_site_get (site,mysite) == 0)
    {
        if(mysite->cattype == CELL_TYPE)
          sprintf(desc,"%d ",mysite->ccat);
        else if(mysite->cattype == FCELL_TYPE || mysite->cattype == DCELL_TYPE)
          sprintf(desc,"%g ",mysite->fcat);
        for(i=0;i<d && strlen(desc) < 30 ;++i)
        {
          sprintf(buf,"%g ",mysite->dbl_att[i]);
          G_strcat(desc,buf);
        }
        for(i=0;i<s && strlen(desc) < 30 ;++i)
        {
          sprintf(buf,"%s ",mysite->str_att[i]);
          G_strcat(desc,buf);
        }
        feature++;
        if (write_moss_header (moss,feature,ncoord,desc,proj) < 0 || 
            write_moss_coordinates (moss,ncoord,&mysite->east,&mysite->north,
                                    isle_flag,proj) < 0)
        {
            fprintf (stdout,"\nNot able to write MOSS import file\n");
            exit (-1);
        }
    }

    fprintf (stdout,"\n%d Sites Converted\n",feature);
    return (0);
}
