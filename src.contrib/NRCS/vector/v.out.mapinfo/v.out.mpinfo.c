/*
*
*  THIS PROGRAM CONVERTS GRASS VECTOR DATA INTO MAPINFO IMPORT FORMAT. 
*  ONLY ONE DATA TYPE CAN BE CONVERTED AT A TIME (SITE, LINE, OR AREA),
*  PRODUCING A MAPINFO IMPORT FILE IN THE MAPSET ELEMENT MAPINFO.
*
*  WRITTEN BY:
*  R.L. Glenn, USDA, NRCS, 9/19/95
*/

#include  "Vect.h"
#include  "gis.h"

#define MAPINFO "mpinfo"
#define UNREF 0
#define UTM 1
#define STP 2
#define LATLONG 3

main(argc, argv)
    int argc;
    char *argv[];
{
    char *mapset,name[50],mif[50], mid[50],resp[2];
    char errmsg[200], buf[100], *ptr;
    FILE *site,*mpmif,*mpmid;    /* POINTERS TO FILES */
    int err,type,site_flag;
    int proj;                 /* PROJECTION TYPE */
    int NAD;                  /* NAD datum */
    int zone;                  /* CURRENT ZONE */

    struct Categories pcats;  /* CATEGORY INFO FOR DIGIT FILE */
    struct Map_info map;      /* SPATIAL INFO FOR DIGIT FILE */

    /* INITIALIZE GIS LIBRARY */
    G_gisinit ("vect.to.mpinfo");

    if (argc == 2 && !strcmp (argv[1], "help"))
    {
	printf ("\n\n");
	printf ("  v.out.mapinfo is an interactive program that converts GRASS vector data to\n");
	printf ("  PC-MapInfo Exchange format. It requires no command line arguments.\n");
	printf ("\n");
	printf ("  The program will ask you to specify:\n\n");
	printf ("     1. Whether the data is (nad27 or nad83);\n");
	printf ("     2. Type of data (site, line, area) and file type (site, vector);\n");
	printf ("     3. Input file name;\n");
	printf ("     4. Output MapInfo Exchange file name (to be placed in mapset element 'mpinfo')\n");
	printf ("\n");

	exit (1);
    }



    /* GET PROJECTION TYPE */
    proj = G_projection();
    if (proj == UNREF)
    {
        printf ("\nUngeo-referenced x,y data can not be converted to MAPINFO\n");
        exit (-1);
    }   

               /* Check for "DATUM_INFO" file */
    G__file_name(buf, "../PERMANENT", "DATUM_INFO", G_mapset()) ;
    if (access(buf,0) == 0) {
       G_lookup_key_value_from_file(buf, "datum", resp, 10);
       ptr = resp;
       ptr+=3;
       NAD = atoi(ptr);
     }
    else {
      printf ("\nIs this data NAD27 (y/n) [y]? "); gets (resp);
      if (resp[0] != 'N' && resp[0] != 'n') NAD=27;
      else NAD=83;
     }

    if (proj != LATLONG) zone = G_zone();
    else if (proj == LATLONG)  zone = 0;
    else {
	printf ("\nThe current projection is not supported,\n");
	printf ("do \"v.proj\" on the data, converting to \n");
	printf ("utm, lat/long, or St.Plane\n");
	exit (-1);
    }

    do
    {
      printf ("\nData type to be converted:\n");
      printf ("\n1   Site");
      printf ("\n2   Line");
      printf ("\n3   Area\n");
      printf ("\nEnter a number <1-3>\nanything else to quit: ");
      gets (resp);
      err = sscanf (resp,"%d",&type);
      if (type < 1 || type > 3 || err != 1) exit(0);
  
      site_flag = type;
      if (site_flag == 1)
      {
         /* GRASS 3.1 AND BEYOND SUPPORTS SITE DATA IN THE DIGIT FILES.  ALLOW USER
          * TO CHOOSE BETWEEN SITE_LISTS OR DIGIT SITES */
          printf ("\nSource of site data:\n");
          printf ("\n1   Site_lists file");
          printf ("\n2   Digit file\n");
          printf ("\nEnter a number <1-2>\nanything else to quit: ");
          gets (resp);
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
              printf ("\nNot able to open site_lists file: %s\n",name);
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
  
      /* PROMPT FOR MAPINFO IMPORT FILE NAME, AND OPEN FILE */
      mapset = G_ask_new (" MAPINFO EXCHANGE FILENAME",name,MAPINFO,"mapinfo exchange");
      if (! mapset) exit (-1);
      
      sprintf(mif,"%s.mif",name);
      sprintf(mid,"%s.mid",name);

      /* ===========  CONVERT THE DATA =========== */

      if ((mpmif = G_fopen_new (MAPINFO,mif)) == NULL)
      {        
          printf ("\nNot able to open mapinfo exchange file %s in element %s\n",
                  mif,MAPINFO);    
          exit (-1);
      }
      if ((mpmid = G_fopen_new (MAPINFO,mid)) == NULL)
      {        
          printf ("\nNot able to open mapinfo exchange file %s in element %s\n",
                  mid,MAPINFO);    
          exit (-1);
      }
  

      if (type == 0)
      {
                  site_to_mpinfo (site,mpmif,mpmid,proj,NAD,zone);
          fclose (site);
      }
      else
      {
          if (type == 1)
                      point_to_mpinfo (&map,&pcats,mpmif,mpmid,proj,NAD,zone);
          if (type == 2)
/*
printf("-->           line_to_mpinfo (&map,&pcats,mpmif,proj);  WORK IN PROGRESS\n");
*/
                      line_to_mpinfo (&map,&pcats,mpmif,mpmid,proj,NAD,zone);
          else if (type == 3) 
              area_to_mpinfo (&map,&pcats,mpmif,mpmid,proj,NAD,zone);
      fclose (mpmif);
      fclose (mpmid);
      }
 

#ifdef OLD_VLIB
          dig_P_fini (&map);
#else
	  Vect_close (&map);
#endif /*OLD_VLIB*/
      G_free_cats (&pcats);



      printf ("\nDo you want to convert another file (y/n) [y]? ");
      gets (resp);
  }
  while (resp[0] != 'N' && resp[0] != 'n');

  exit (0);
}      
