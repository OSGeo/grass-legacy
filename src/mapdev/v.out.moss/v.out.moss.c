/*
*
*  THIS PROGRAM CONVERTS GRASS VECTOR DATA INTO MOSS IMPORT FORMAT.  ONLY ONE DATA TYPE
*  CAN BE CONVERTED AT A TIME (SITE, LINE, OR AREA), PRODUCING A MOSS IMPORT FILE IN THE
*  MAPSET ELEMENT MOSS.
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include  "Vect.h"
#include  "gis.h"
#include  "moss.h"

#define MOSS "moss"
#define UNREF 0

int main (int argc, char *argv[])
{
    char *mapset,name[50],resp[2];
	char errmsg[200];
    FILE *site,*moss;         /* POINTERS TO FILES */

    int err,type,site_flag;
    int proj;                 /* PROJECTION TYPE */

    struct Categories pcats;  /* CATEGORY INFO FOR DIGIT FILE */
    struct Map_info map;      /* SPATIAL INFO FOR DIGIT FILE */

    /* INITIALIZE GIS LIBRARY */
    G_gisinit (argv[0]);

    if (argc == 2 && !strcmp (argv[1], "help"))
    {
	fprintf (stdout,"\n\n");
	fprintf (stdout,"  v.out.moss is an interactive program that converts GRASS vector data to\n");
	fprintf (stdout,"  Moss Import format. It requires no command line arguments.\n");
	fprintf (stdout,"\n");
	fprintf (stdout,"  The program will ask you to specify:\n\n");
	fprintf (stdout,"     1. Type of data (site, line, area) and file type (site, vector);\n");
	fprintf (stdout,"     2. Input file name;\n");
	fprintf (stdout,"     3. Output Moss Import file name (to be placed in mapset element 'moss')\n");
	fprintf (stdout,"\n");

	exit (1);
    }



    /* GET PROJECTION TYPE */
    proj = G_projection();
    if (proj == UNREF)
    {
        fprintf (stdout,"\nUnreferenced x,y data can not be converted to MOSS\n");
        exit (-1);
    }   

    do
    {
      fprintf (stdout,"\nData type to be converted:\n");
      fprintf (stdout,"\n1   Site");
      fprintf (stdout,"\n2   Line");
      fprintf (stdout,"\n3   Area\n");
      fprintf (stdout,"\nEnter a number <1-3>\nanything else to quit: ");
      fgets (resp,2,stdin);
      err = sscanf (resp,"%d",&type);
      if (type < 1 || type > 3 || err != 1) exit(0);
  
      site_flag = type;
      if (site_flag == 1)
      {
         /* GRASS 3.1 AND BEYOND SUPPORTS SITE DATA IN THE DIGIT FILES.  ALLOW USER
          * TO CHOOSE BETWEEN SITE_LISTS OR DIGIT SITES */
          fprintf (stdout,"\nSource of site data:\n");
          fprintf (stdout,"\n1   Site_lists file");
          fprintf (stdout,"\n2   Digit file\n");
          fprintf (stdout,"\nEnter a number <1-2>\nanything else to quit: ");
          fgets (resp,2,stdin);
          err = sscanf (resp,"%d",&type);
          if (type < 1 || type > 2 || err != 1) exit(0);
          type--;
      }

      /* INITIALIZE BASED ON THE DATA TYPE */
      if (type == 0)
      {
          /* PROMPT FOR SITE_LISTS NAME, AND OPEN FILE */
          mapset = G_ask_sites_old (" SITE FILE TO CONVERT ",name);
          if (! mapset) exit (-1);
          
          if ((site = G_fopen_sites_old (name,mapset)) == NULL)
          {
              fprintf (stdout,"\nNot able to open site_lists file: %s\n",name);
              exit (-1);
          }
      }
      else
      {
          /* PROMPT FOR DIGIT FILE NAME */
          mapset = G_ask_vector_old (" VECTOR (DIGIT) FILE TO CONVERT ",name);
          if (! mapset) exit (-1);

#ifdef OLD_VLIB
          
          /* INTIALIZE LEVEL TWO DIGIT LIBRARY ACCESS */
          dig_P_init (name,mapset,&map);
#else
          if (0 > Vect_open_old (&map, name, mapset))
           {
               sprintf(errmsg, "Not able to open vector file <%s>\n", name) ;
               G_fatal_error (errmsg);
           }

#endif /*OLD_VLIB*/

          /* INITIALIZE CATS ARRAY */
          G_suppress_warnings (1);
          G_read_vector_cats (name,mapset,&pcats);
          G_suppress_warnings (0);
      }
  
      /* PROMPT FOR MOSS IMPORT FILE NAME, AND OPEN FILE */
      mapset = G_ask_new (" MOSS IMPORT FILENAME",name,MOSS,"moss import");
      if (! mapset) exit (-1);

      if ((moss = G_fopen_new (MOSS,name)) == NULL)
      {        
          fprintf (stdout,"\nNot able to open moss import file %s in element %s\n",
                  name,MOSS);    
          exit (-1);
      }
  
       
      /* CONVERT THE DATA */
      if (type == 0)
      {
          site_to_moss (site,moss,proj);
          fclose (site);
      }
      else
      {
          if (type == 1)
              point_to_moss (&map,&pcats,moss,proj);
          if (type == 2)
              line_to_moss (&map,&pcats,moss,proj);
          else if (type == 3) 
              area_to_moss (&map,&pcats,moss,proj);

#ifdef OLD_VLIB
          dig_P_fini (&map);
#else
		  Vect_close (&map);
#endif /*OLD_VLIB*/
          G_free_cats (&pcats);
      }
          
      fclose (moss);

      fprintf (stdout,"\nDo you want to convert another file (y/n) [y]? ");
      fgets (resp,2,stdin);
  }
  while (resp[0] != 'N' && resp[0] != 'n');

  exit (0);
}      
