/* %W% %G% */
/* main.c    1.1   03/19/91  GRASS4.0
/* main.c    1.0   11/04/90
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of changing to a new projection
*             to match disparent mapset data
*
*  ------Rev 3.1 arguements --------------------------------------
*    Input arguements:
*           Vproj name=map_name proj=projection_name 
*
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.proj map=vector file to read
*                    inset=mapset for INput vector file default current
*                    outset=mapset for output vector file default current
*
*/

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include  "gis.h"
#define MAIN

#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"


main (argc,argv)
int argc;
char *argv[];

{
	int i, in_stat, out_stat, cnt=0, type, cat, cmd1_cnt = 0, cmd2_cnt = 0;
	int day, yr, Out_proj;
	char ctype[1];
	char out_lon0[5], out_lat0[5];
        char answer[50], buffa[300], buffb[300], *value1;
        char *mapset, *omapset, *new_data, *G_tempfile(), *gets(), *tmpfile;
	char cmnd1[200], cmnd2[200] ; /* proj options ****/
	char omap_name[20], map_name[20], iset_name[20], oset_name[20];
	char proj_out[50], proj_in[50];
	char opath[256], ipath[256], *value, *key ;
	FILE *tf;
        long offset;
        struct Option *osetopt, *mapopt, *isetopt;
        struct Key_Value *in_proj_keys;
        struct Key_Value *out_proj_keys;

        G_gisinit (argv[0]);
     
		 /* set up the options and flags for the command line parser */

        mapopt = G_define_option();
        mapopt->key             = "map";
        mapopt->type            =  TYPE_STRING;
        mapopt->required        =  YES;
        mapopt->description     = "input vector file name";

        isetopt = G_define_option();
        isetopt->key             =  "inset";
        isetopt->type            =  TYPE_STRING;
        isetopt->required        =  NO;
        isetopt->description     =  "mapset containing INput vector map";

        osetopt = G_define_option();
        osetopt->key             =  "outset";
        osetopt->type            =  TYPE_STRING;
        osetopt->required        =  NO;
        osetopt->description     =  "mapset to contain OUTput vector map";

 
	   /* heeeerrrrrre's the   PARSER */
        if (G_parser (argc, argv))
		    exit (-1);
		     
           /* start checking options and flags */

									  
           /* set input vector file name and mapset */
	sprintf(map_name,"%s",mapopt->answer);

	if (isetopt->answer)
		sprintf(iset_name,"%s",isetopt->answer);
	else
		sprintf(iset_name,"%s",G_mapset());
	if (osetopt->answer)
		sprintf(oset_name,"%s",osetopt->answer);
	else 
		sprintf(oset_name,"%s",G_mapset());

	if (strcmp(iset_name,oset_name) == 0)
		G_fatal_error("Input and output mapsets can not be the smae\n");

           /* Make sure map is available */
	mapset = G_find_vector (map_name, iset_name) ;
	if (mapset == NULL)
	{
		sprintf(buffb,"Vector file [%s] not available",map_name);
		G_fatal_error(buffb) ;
	}

           /* See if map is already in output mapset */
	omapset = G_find_vector (map_name, oset_name) ;
	if (omapset != NULL)
	{
		sprintf(buffb,"Vector file [%s] already in mapset [%s]",
					 map_name, oset_name);
		G_fatal_error(buffb) ;
	}

	sprintf(opath,"%s/%s",G_location_path(),oset_name);
	if (access(opath,0) != 0)
	   {
	   fprintf(stderr,
		 "\nMapset [%s] does NOT exist, creating new mapset\n",opath);
	   sprintf(buffb,"mkdir %s\n",opath);
	   system(buffb);
	   }
	sprintf(buffa,"%s/dig",opath);
	if (access(buffa,0) != 0)
	   {
	   sprintf(buffb,"mkdir %s/dig\n",opath);
	   system(buffb);
	   }
	sprintf(buffa,"%s/dig_att",opath);
	if (access(buffa,0) != 0)
	   {
	   sprintf(buffb,"mkdir %s/dig_att\n",opath);
	   system(buffb);
	   }
	sprintf(buffa,"%s/dig_cats",opath);
	if (access(buffa,0) != 0)
	   {
	   sprintf(buffb,"mkdir %s/dig_cats\n",opath);
	   system(buffb);
	   }

           /*** Get projection info for input mapset ***/
          G__file_name (ipath, "", PROJECTION_FILE, iset_name);
          while (access(ipath,0) != 0)
            {
            fprintf(stderr,"ifile %s\n",ipath);
            fprintf(stderr,"PROJ_INFO file not found  for mapset %s\n",
								  iset_name);
            fprintf(stderr,
		     "Projections are: utm,aea,stp,ll,lcc,merc,tmerc,xxx\n");
            fprintf(stderr,"Enter projection for input mapset : ");
            scanf("%s",proj_in);
            fprintf(stderr,"Running m.setproj\n");
            sprintf(buffb,"m.setproj set=%s proj=%s",iset_name,proj_in);
            system(buffb);
            }
          in_proj_keys = G_read_key_value_file(ipath,&in_stat);
          if (in_stat != 0)
            {
            sprintf(buffb,"ERROR in reading current mapset PROJ_INFO \n");
            G_fatal_error(buffb) ;
            }
          sprintf(proj_in,"%s", G_find_key_value("name",in_proj_keys));

/************** END of input projection **********************/

	     /****** get the output projection parameters ******/
        G__file_name (opath,"",PROJECTION_FILE,oset_name);
        while (access(opath,0) != 0)
          {
          fprintf(stderr,"PROJ_INFO file not found  for mapset %s\n",oset_name);
          fprintf(stderr,
			"Projections are: utm,aea,stp,ll,lcc,merc,tmerc,xxx\n");
          fprintf(stderr,"Enter projection for out mapset : ");
          scanf("%s",proj_out);
          fprintf(stderr,"Running m.setproj\n");
          sprintf(buffb,"m.setproj set=%s proj=%s",oset_name,proj_out);
          system(buffb);
          }
        out_proj_keys = G_read_key_value_file(opath, &out_stat);
          
        if (out_stat != 0)
          {
          sprintf(buffb,"PROJ_INFO file in %s is unreadable\n",
          oset_name);
          G_fatal_error(buffb) ;
          }
        sprintf(proj_out,"%s", G_find_key_value("name",out_proj_keys));

/***************** END of output projection **********************/

if (strcmp(proj_in,"Lat/Long") == 0) in_stat = 2;
if (strcmp(proj_out,"Lat/Long") == 0) out_stat = 2;


/********** create input proj parameters **********************/

	cmnd1[0] = '\0';
        if (in_stat != 2)    /* this is NOT a ll mapset */
          { /* read values and create input proj command buffer */
	    for (i=1; i<=in_proj_keys->nitems-1; i++)
	       {
	       sprintf(buffa,"+%s=%s\t",
	       in_proj_keys->key[i],in_proj_keys->value[i]);
	       strcat(cmnd1,buffa);
	       }
	    strcat (cmnd1, "+inv\0");
            cmd1_cnt = in_proj_keys->nitems + 1;
	    G_free_key_value(in_proj_keys);
          }

/***************** End of proj parameter creation **************/

/********** create output proj parameters **********************/
	cmnd2[0] = '\0';
        if (out_stat != 2)
	   {               /* got one  */
	       /* read values and create output command buffer */
  	   if ((value1 = G_find_key_value("name",out_proj_keys)) == NULL)
			G_fatal_error("Could not find proper key\n");
	   if (strcmp(value1,"Lat/Long") != 0)
	      {
	      for (i=1; i<out_proj_keys->nitems-1; i++)
	        {
	        sprintf(buffa,"+%s=%s\t",
	        out_proj_keys->key[i],out_proj_keys->value[i]);
	        strcat(cmnd2,buffa);
	        }
              i = out_proj_keys->nitems - 1;
	      sprintf(buffa,"+%s=%s\0",
		   out_proj_keys->key[i],out_proj_keys->value[i]);
	      strcat(cmnd2,buffa);
              cmd2_cnt = out_proj_keys->nitems;
	      G_free_key_value(out_proj_keys);
	      }
	   }

/***************** End of proj parameter creation **************/

tmpfile = G_tempfile();
/******************** DO projection change *********************/
if ((cmd1_cnt > 0 ) && (cmd2_cnt > 0)) 
	  {    /********** Do INVERSE *****************************/
	  sprintf(omap_name,"pj.%d",getpid());
	  if ((tf = fopen(tmpfile,"w")) == NULL) {
		sprintf(buffb,"Can,t open temp file %s\n",tmpfile);
		G_fatal_error(buffb);
		}
	  fprintf(tf,"%s\n",cmnd1);
	  fclose(tf);
      if (strcmp(proj_in,"State Plane") == 0) 
	  	sprintf(buffb,
	       "%s/etc/v.projINV map=%s inset=%s outset=%s omap=%s parms=%s stp=yes\n",
	        G_gisbase(),map_name,iset_name,G_mapset(),omap_name,tmpfile);
	else
	  sprintf(buffb,
	       "%s/etc/v.projINV map=%s inset=%s outset=%s omap=%s parms=%s\n",
	        G_gisbase(),map_name,iset_name,G_mapset(),omap_name,tmpfile);
	  if (system(buffb) != 0)
		G_fatal_error("Could not do Inverse projection\n");
/************ DO forward **************************/
	  if ((tf = fopen(tmpfile,"w")) == NULL) {
		sprintf(buffb,"Can,t open temp file %s\n",tmpfile);
		G_fatal_error(buffb);
		}
	  fprintf(tf,"%s\n",cmnd2);
	  fclose(tf);
      if (strcmp(proj_out,"State Plane") == 0) 
	  	sprintf(buffb,
	       "%s/etc/v.projFOR map=%s inset=%s outset=%s omap=%s parms=%s stp=yes\n",
		G_gisbase(),omap_name,G_mapset(),oset_name,map_name,tmpfile);
	else
	  sprintf(buffb,
		"%s/etc/v.projFOR map=%s inset=%s outset=%s omap=%s parms=%s\n",
		G_gisbase(),omap_name,G_mapset(),oset_name,map_name,tmpfile);
	  if (system(buffb) != 0)
		G_fatal_error("Could not do Forward projection\n");
	  }
/******************************** END BOTH ********************/

/******************** ONLY cmnd1 *****************************/
else if (cmd1_cnt > 0) 
	  {
	  if ((tf = fopen(tmpfile,"w")) == NULL) {
		sprintf(buffb,"Can,t open temp file %s\n",tmpfile);
		G_fatal_error(buffb);
		}
	  fprintf(tf,"%s\n",cmnd1);
	  fclose(tf);
      if (strcmp(proj_in,"State Plane") == 0) 
	  	sprintf(buffb,
	       "%s/etc/v.projINV map=%s inset=%s outset=%s omap=%s parms=%s stp=yes\n",
		G_gisbase(), map_name,iset_name,oset_name,map_name,tmpfile);
	else
	  sprintf(buffb,
		"%s/etc/v.projINV map=%s inset=%s outset=%s omap=%s parms=%s\n",
		G_gisbase(), map_name,iset_name,oset_name,map_name,tmpfile);
	  if (system(buffb) != 0)
		G_fatal_error("Could not do Inverse projection\n");
	  }

/***************** ONLY cmnd2   **********************************/
else if (cmd2_cnt > 0) 
	  {
	  if ((tf = fopen(tmpfile,"w")) == NULL) {
		sprintf(buffb,"Can,t open temp file %s\n",tmpfile);
		G_fatal_error(buffb);
		}
	  fprintf(tf,"%s\n",cmnd2);
	  fclose(tf);
      if (strcmp(proj_out,"State Plane") == 0) 
	  	sprintf(buffb,
	       "%s/etc/v.projFOR map=%s inset=%s outset=%s omap=%s parms=%s stp=yes\n",
		G_gisbase(), map_name,iset_name,oset_name,map_name,tmpfile);
	else
	  sprintf(buffb,
		"%s/etc/v.projFOR map=%s inset=%s outset=%s omap=%s parms=%s\n",
		G_gisbase(), map_name,iset_name,oset_name,map_name,tmpfile);
	  if (system(buffb) != 0)
		G_fatal_error("Could not do Forward projection\n");
	  }

if ((cmd1_cnt > 0 ) && (cmd2_cnt > 0))
	  {
	  sprintf(buffb,"g.remove vect=%s > /dev/null",omap_name);
	  system(buffb);
	  }
sprintf(buffb,"rm %s",tmpfile);
system(buffb);
fprintf(stderr, "\n\n%s of vector file <%s> has completed\n",
argv[0],map_name);
fprintf(stderr, "vector file <%s> in mapset <%s> will require\n",map_name,oset_name);
fprintf(stderr, "  v.support be run, before the data is usable\n");
}
