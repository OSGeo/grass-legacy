/* Function: "main.c" for GRASS Program "v.bubble".               */
/* "v.bubble" creates vector "circle" polygons using points from  */
/* the site_lists files as the centers of the those "polygon"     */
/* circles. The radius of the circle correpsonds with the z       */
/* values in the sitefile                                         */

/* The code of this program is partially taken from v.circle and  */
/* s.univar                                                       */


/* Exit status of 0 indicates program was successful.     */
/* Exit status of 1 indicates program was not successful. */

/* --==<< CHANGES >>==--

  $Id$
  
  1 feb 20000 : Job Spijker spijker@geo.uu.nl
  initial program
 */


#include "v.bubble.h"


int main( int argc, char **argv )
 {
  char *vect_mapset;
  char *site_mapset;
  struct Option *opt_radius;
  struct Option *opt_field;
  struct Option *opt_input;
  struct Option *opt_output;
  int scan_int;
  double radius;
  int dec_field;
  char input[1024], output[1024];
  char fqn_output[1024];
  char *fqn;
  FILE *fd_site;
  struct line_pnts *pnts;
  char command[1024];
  struct Flag *flag;
  SITE_XYZ *bsite;
  int nsites; /*number of sites*/
  int i1,i2,i3;
  struct Map_info map;
  
  

/* Initialize the GIS calls */
  G_gisinit(argv[0]) ;


/* Request a pointer to memory for each option. */
  opt_input = G_define_option();
  opt_input->key = "sitefile";
  opt_input->type = TYPE_STRING;
  opt_input->required = YES;
  opt_input->gisprompt = "old,site_lists,site_lists";
  opt_input->description = "GRASS site_lists file (input).";
  opt_input->answer = "";

/* Request a pointer to memory for each option. */
  opt_output = G_define_option();
  opt_output->key = "output";
  opt_output->type = TYPE_STRING;
  opt_output->required = YES;
  opt_output->gisprompt = "any,dig,vector";
  opt_output->description = "Vector file to be created (output).";
  opt_output->answer = "";

/* Request a pointer to memory for each option. */
  opt_radius = G_define_option();
  opt_radius->key = "radius";
  opt_radius->type = TYPE_DOUBLE;
  opt_radius->required = NO;
  opt_radius->description = "Maximum radius corresponding with maximum z value in the sitemap, unit is units of map\n";
  opt_radius->answer = "1.0";

/* Request a pointer to memory for each option. */
  opt_field = G_define_option();
  opt_field->key = "field";
  opt_field->type = TYPE_INTEGER;
  opt_field->required = NO;
  opt_field->description = "Attribute field number to use for operation";
  opt_field->answer = "1";

  /* Request a pointer to memory for each flag. */
  flag = G_define_flag();
  flag->key = 's';
  flag->description = "Automatically run \"v.support\" on newly created \
vector file."; 
  flag->answer = 0x00;


  /* Using GRASS parsing to obtain arguments... */
  if (G_parser(argc, argv) != 0)
   {
    exit(1);
   }

  scan_int=sscanf(opt_radius->answer,"%lf",&radius);
  if (scan_int <= 0)
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect value for radius.\n",
            G_program_name(),opt_radius->answer );
    G_fatal_error (msg);
    exit(1);
   }

  scan_int=sscanf(opt_field->answer,"%d",&dec_field);
  if ((scan_int <= 0) || dec_field < 1)
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect value for attribute field number.\n",
            G_program_name(),opt_field->answer );
    G_fatal_error (msg);
    exit(1);
   }

/* Make sure that the current projection is UTM or defined-99 or  */
/* unreferenced XY projection.                       */
  if ((G_projection() != 0)&&(G_projection() != 1) &&(G_projection() != 99))
   {
    char msg[256];      
    sprintf(msg,"%s:  Projection must be either Unreferenced XY (value 0) or \
UTM (value 1).  Change the value \"proj\" in file \"WIND\" to either \
0 or 1 and then re-executed \"%s\".",
    G_program_name(), G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

      /* Make sure that radius is not less than 0.0 . */
      if (radius < 0.0)
       {
        char msg[256];      
        sprintf(msg,"%s: \"%s\" is an incorrect value for radius.\n",
                G_program_name(),opt_radius->answer );
        G_fatal_error (msg);
        exit(1);
       }

  /* input file name */
  strcpy(input,opt_input->answer);

  /* output file name */
  strcpy(output,opt_output->answer);

  /* Obtain the mapset name for the chosen site_lists file "input". */
  if ((site_mapset = G_find_file("site_lists",input,""))==NULL)
   {
    char msg[256];
    sprintf(msg,"%s: Site_lists file: <%s> does not exist.",
    G_program_name(), input);
    G_fatal_error (msg);
    exit(1);
   }

  /* Get "mapset". */
  vect_mapset = G_mapset();

/* Make sure that vector file "output" does not already exist in "vect_mapset". */
  if ( (vect_mapset = G_find_vector2(output,vect_mapset)) != NULL )
   {
    char msg[256];
    sprintf(msg,"%s: Vector file name: <%s> already exists in mapset \"%s\".  \
Please choose a different vector file name.\n",
    G_program_name(), output, G_mapset() );
    G_fatal_error (msg);
    exit(1);
   }

  fqn = fqn_output;
  fqn = G_fully_qualified_name(output,vect_mapset);
   
  /* Open site_lists file. */
  fd_site = G_fopen_sites_old(input,site_mapset);
  if (fd_site == NULL)
   {
    char msg[256];
    sprintf(msg,"%s: Unable to open site_lists file <%s> in mapset \"%s\".",
    G_program_name(),output,site_mapset );
    G_fatal_error (msg);
    exit(1);
   }

  if ( (Vect_open_new(&map,output) < 0) )
   {
    char msg[256];
    sprintf(msg,"%s: Unable to open vector file <%s> in mapset \"%s\".",
    G_program_name(),output,vect_mapset );
    G_fatal_error (msg);
    exit(1);
   }

  /* Provide warning message if projection is Unreferenced XY. */
  if (G_projection() == 0)
   {
    fprintf(stderr,"WARNING:\n\
%s:  The coordinates for the vector output file\n\
<%s> will be in meters.\n",
    G_program_name(), output );
   }

  pnts = Vect_new_line_struct();

   /* How many Sites max ? */
   {
        char site_line[MAX_SITE_LEN];
        nsites = 0;
        while(NULL != fgets(&(site_line[0]), MAX_SITE_LEN, fd_site))
            nsites++;
        if (!nsites)
            G_fatal_error("No sites found.");
        rewind(fd_site);
    }
   /* Get the SITES_XYZ */
   {
        if (NULL == (bsite = G_alloc_site_xyz(nsites)))
            G_fatal_error ("Out of Memory!");
        nsites = G_readsites_xyz(fd_site, SITE_COL_DBL, dec_field, 
                nsites, NULL, &(bsite[0]));
        if (nsites <= 0)
            G_fatal_error ("Unable to read sites list.");
        else
            fprintf(stderr, "%d sites found\n", nsites);
        
    }
   
   i1=bubbling(bsite,nsites,&map,radius);
  
   G_free_site_xyz(bsite);
   
  fclose(fd_site);
  
  /* If "-s" flag is passed as argument then run "v.support" on */
  /* newly created vector file (output).                        */
  if (flag->answer == 0x01)
   {
    sleep(8);
    sprintf(command,"%s/bin/v.support map=%s",G_gisbase(), output );
    system(command);
   }
  printf("Done ...\n\n");
  exit(0);
 }


