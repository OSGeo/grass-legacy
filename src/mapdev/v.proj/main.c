#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "V_.h"
#include "projects.h"
#include "local_proto.h"

#define	B_DIR  "dig"
#define	ATT_DIR  "dig_att"
#define	CAT_DIR  "dig_cats"
/*
#define PROJECTION_FILE "PROJ_INFO"
#define UNIT_FILE "PROJ_UNITS"
*/

int main (int argc, char *argv[])
{
	int i, type, cat, vect_read, stat, cnt;
	int day, yr, Out_proj;
	int out_zone = 0;
	char ctype[3];
	char out_lon0[5], out_lat0[5];
        char answer[50], buffa[1024], buffb[1024], *value1;
        char *mapset, *omapset, *new_data, *tmpfile;
	char *omap_name, *map_name, *iset_name, *oset_name, *iloc_name;
        struct pj_info info_in;
        struct pj_info info_out;
	char proj_out[50], proj_in[50];
        char *gbase;
        static char *oform = (char *)0;
	char opath[1024];
	char att_file[100], cat_file[100], date[40], mon[4];
	FILE *wnd;
        struct Option *omapopt, *mapopt, *isetopt, *ilocopt, *ibaseopt;
        struct Key_Value *in_proj_keys, *in_unit_keys;
        struct Key_Value *out_proj_keys, *out_unit_keys;
        double HE, HW, HS, HN, E, W, S, N, X, Y, num;
        FILE *in, *out1;
        struct line_pnts *Points;
        struct Map_info Map;
        struct Map_info Out_Map;

        G_gisinit (argv[0]);
     
		 /* set up the options and flags for the command line parser */

        mapopt = G_define_option();
        mapopt->key             = "input";
        mapopt->type            =  TYPE_STRING;
        mapopt->required        =  YES;
        mapopt->description     = "input vector map";

        ilocopt = G_define_option();
        ilocopt->key             =  "location";
        ilocopt->type            =  TYPE_STRING;
        ilocopt->required        =  YES;
        ilocopt->description     =  "location containing input vector map";

        ibaseopt = G_define_option();
        ibaseopt->key             =  "dbase";
        ibaseopt->type            =  TYPE_STRING;
        ibaseopt->required        =  NO;
        ibaseopt->description     =  "path to GRASS database of input location";

        isetopt = G_define_option();
        isetopt->key             =  "mapset";
        isetopt->type            =  TYPE_STRING;
        isetopt->required        =  NO;
        isetopt->description     =  "mapset containing input vector map";

        omapopt = G_define_option();
        omapopt->key             = "output";
        omapopt->type            =  TYPE_STRING;
        omapopt->required        =  NO;
        omapopt->description     = "output vector map";

/*
        osetopt = G_define_option();
        osetopt->key             =  "outset";
        osetopt->type            =  TYPE_STRING;
        osetopt->required        =  NO;
        osetopt->description     =  "mapset to contain OUTput vector map";
*/
 
	   /* heeeerrrrrre's the   PARSER */
        if (G_parser (argc, argv))
		    exit (-1);
		     
           /* start checking options and flags */

									  
           /* set input vector file name and mapset */
	map_name = mapopt->answer;
	if (omapopt->answer)
		omap_name = omapopt->answer;
	else
		omap_name = map_name;

	if (isetopt->answer)
		iset_name = isetopt->answer;

	else
		iset_name = G_store (G_mapset());

        oset_name = G_store (G_mapset());

	iloc_name = ilocopt->answer;

	if (ibaseopt->answer)
		gbase = ibaseopt->answer;
	else
		gbase = G_store (G_gisdbase());

	if (strcmp(iloc_name,G_location()) == 0)
             G_fatal_error("Input and output locations can not be the same\n");

           /* Make sure map is available */
/*
	mapset = G_find_vector (map_name, iset_name) ;
	if (mapset == NULL)
	{
		sprintf(buffb,"Vector file [%s] not available",map_name);
		G_fatal_error(buffb) ;
	}
*/

           /* See if map is already in output mapset */
	omapset = G_find_vector (omap_name, oset_name) ;
	if (omapset != NULL)
	{
		sprintf(buffb,"Vector file [%s] already in mapset [%s]",
					 omap_name, oset_name);
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
/* Change the location here and then come back */
	
           select_target_env();
           G__setenv ("GISDBASE", gbase);
           G__setenv ("LOCATION_NAME", iloc_name);
           stat = G__mapset_permissions(iset_name);

/*DEBUG*/
{
	char path[256];
        G__file_name (path,"","",iset_name);
/*
	fprintf (stderr, "MAPSET: '%s'\n", path);
*/

}
           if (stat >= 0)
           {
        	G__setenv ("MAPSET", iset_name);
                /* Make sure map is available */
	        mapset = G_find_vector (map_name, iset_name) ;
        	if (mapset == NULL)
        	{
		    sprintf(buffb,"Vector file [%s] not available",map_name);
	            G_fatal_error(buffb) ;
	         }
           /*** Get projection info for input mapset ***/
                 in_proj_keys = G_get_projinfo();
                 if (in_proj_keys == NULL) {
                   exit (0);
                 } 
                 in_unit_keys = G_get_projunits();
                 if (in_unit_keys == NULL) {
                   exit (0);
                 } 
                 if (pj_get_kv(&info_in,in_proj_keys,in_unit_keys) < 0) {
                   exit (0);
                 }
                 if ((vect_read = Vect_open_old(&Map,map_name, mapset)) < 0 )
                 {
                    G_fatal_error("Reading input file.") ;
                    exit(-1) ;
                 }
                 if (vect_read < 1)
                 {
                    G_fatal_error("You must run v.support on this file.") ;
                    exit(-1) ;
                 }
                 G__file_name(att_file, ATT_DIR, map_name, mapset);
	         in = fopen (att_file, "r");
                 G__file_name(cat_file, CAT_DIR, map_name, mapset);
           }
           else if (stat < 0)	/* allow 0 (i.e. denied permission) */
				/* need to be able to read from others */
           {
             sprintf (buffb, "Mapset [%s] in input location [%s] - ",
	                                                iset_name, iloc_name);
             strcat (buffb,stat == 0 ? "permission denied\n" : "not found\n");
	     G_fatal_error(buffb) ;
           }
/*
             else {
             sprintf (buffb, "No changes yet for permission denied\n ");
	     G_fatal_error(buffb) ;
           }
*/

           select_current_env();


	     /****** get the output projection parameters ******/
           Out_proj = G_projection();
           out_proj_keys = G_get_projinfo();
           if (out_proj_keys == NULL) {
             exit (0);
           } 
           out_unit_keys = G_get_projunits();
           if (out_unit_keys == NULL) {
             exit (0);
           } 
           if (pj_get_kv(&info_out,out_proj_keys,out_unit_keys) < 0) {
             exit (0);
           } 
           G_free_key_value(in_proj_keys);
           G_free_key_value(in_unit_keys);
           G_free_key_value(out_proj_keys);
           G_free_key_value(out_unit_keys);

	   if ((vect_read =  scs_open_new(&Out_Map, omap_name, oset_name)) < 0) 
	   {
	      sprintf(buffb,"Can't create new output file <%s in %s> \n", 
                                               omap_name, oset_name) ;
	      G_fatal_error(buffb) ;
	   }
        out_zone = info_out.zone;
        sscanf(info_out.proj,"%s",proj_out);


                     /* Read and write header info */
        sprintf(date,"%s",G_date());
        sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
        if (yr < 2000) yr = yr - 1900;
        else yr = yr - 2000;
        sprintf(date,"%s %d %d",mon,day,yr);
        Vect_copy_head_data(&Map.head, &Out_Map.head);	

	strcpy(Out_Map.head.date,date);
	strcpy(Out_Map.head.your_name,omap_name);
	strcpy(Out_Map.head.line_3,buffa);

        fprintf(stderr,"\nCreating dig file...\n");

             /* Do the header portion */
        HE = Map.head.E;
        HS = Map.head.S;
        HW = Map.head.W;
        HN = Map.head.N;

/*DEBUG 	fprintf(stderr,"N_%f S_%f E_%f W_%f\n",HN,HS,HE,HW);*/ 

        oform = "%.10f";

        /*SE*/
        if(pj_do_proj(&HE,&HS,&info_in,&info_out)<0) {
          fprintf(stderr,"Error in pj_do_proj\n");
          exit(0);
        }
        E = HE;
        S = HS;
        HE = Map.head.E;
        HS = Map.head.S;

        /*NE*/
        if(pj_do_proj(&HE,&HN,&info_in,&info_out)<0) {
          fprintf(stderr,"Error in pj_do_proj\n");
          exit(0);
        }
        N = HN;
        HN = Map.head.N;
        if (HE < E) Out_Map.head.E = E;
        else Out_Map.head.E = HE;

        /*SW*/
        if(pj_do_proj(&HW,&HS,&info_in,&info_out)<0) {
          fprintf(stderr,"Error in pj_do_proj\n");
          exit(0);
        }
        W = HW;
        if (S < HS) Out_Map.head.S = S;
        else Out_Map.head.S = HS;
        HN = Map.head.N;
        HW = Map.head.W;

        /*NW*/
        if(pj_do_proj(&HW,&HN,&info_in,&info_out)<0) {
          fprintf(stderr,"Error in pj_do_proj\n");
          exit(0);
        }
        if (HN < N) Out_Map.head.N = N;
        else Out_Map.head.N = HN;
        if (HW > W) Out_Map.head.W = W;
        else Out_Map.head.W = HW;
/*DEBUG  	fprintf(stderr,"N_%f S_%f E_%f W_%f\n",HN,HS,HE,HW); */ 
/*DEBUG 	fprintf(stderr,"N_%f S_%f E_%f W_%f\n\n",N,S,E,W);   */

	Out_Map.head.plani_zone = out_zone;
/* DEBUG */
	   num = (int)(Out_Map.head.N +1);
           G_format_northing (num,buffb,Out_proj);
           fprintf (stderr, "north:      %s\n", buffb);
	   num = (int)(Out_Map.head.S -1);
           G_format_northing (num,buffb,Out_proj);
           fprintf (stderr, "south:      %s\n", buffb);
	   num = (int)(Out_Map.head.E +1);
           G_format_easting (num,buffb,Out_proj);
           fprintf (stderr, "east:       %s\n", buffb);
	   num = (int)(Out_Map.head.W -1);
           G_format_easting (num,buffb,Out_proj);
           fprintf (stderr, "west:       %s\n", buffb);
/* DEBUG */
		/* create WIND file, if not existing */
	sprintf(buffa,"%s/WIND",opath);
	if (access(buffa,0) != 0)
	   {
	   if (strncmp(proj_out,"ll",2) == 0) Out_proj = 3;
	   else Out_proj = 99;
           if ((wnd = fopen(buffa,"w")) == NULL) 
	        G_fatal_error(" Can not open NEW WIND file\n");
	   fprintf(wnd,"proj:       %d\n",Out_proj);
	   fprintf(wnd,"zone:       %d\n",out_zone);
	   num = (int)(Out_Map.head.N +1);
           G_format_northing (num,buffb,Out_proj);
           fprintf (wnd, "north:      %s\n", buffb);
	   num = (int)(Out_Map.head.S -1);
           G_format_northing (num,buffb,Out_proj);
           fprintf (wnd, "south:      %s\n", buffb);
	   num = (int)(Out_Map.head.E +1);
           G_format_easting (num,buffb,Out_proj);
           fprintf (wnd, "east:       %s\n", buffb);
	   num = (int)(Out_Map.head.W -1);
           G_format_easting (num,buffb,Out_proj);
           fprintf (wnd, "west:       %s\n", buffb);
	   if (Out_proj == 3)
	      i = (int)((Out_Map.head.E - Out_Map.head.W) * 60.);
           else
	      i = (int)((Out_Map.head.E - Out_Map.head.W) / 100.);
	   fprintf (wnd, "cols:       %d\n",i);
	   if (Out_proj == 3)
	      i = (int)((Out_Map.head.N - Out_Map.head.S) * 60.);
           else
	      i = (int)((Out_Map.head.N - Out_Map.head.S) / 100.);
	   fprintf (wnd, "rows:       %d\n",i);
	   fprintf (wnd, "e-w resol:  100\n");
	   fprintf (wnd, "n-s resol:  100\n");
	   fclose(wnd);
           }
  
                     /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();


                     /* Cycle through all lines */
	i = 0;
        while(1)
        {
	    ++i;
                                 /* read line */

	    type = Vect_read_next_line (&Map, Points);
            if (type >= DEAD_LINE) continue;

	    if (type == -1)
			G_fatal_error("Reading input dig file.") ;
	    if ( type == -2) break;
            for (cnt=0; cnt<Points->n_points; cnt++)
            {
              X = Points->x[cnt];
              Y = Points->y[cnt];
              if(pj_do_proj(&X,&Y,&info_in,&info_out)<0) { 
                fprintf(stderr,"Error in pj_do_proj\n");
                exit(0);
              }
              Points->x[cnt] = X;
	      Points->y[cnt] = Y;
            }


                          /* write line */
            Vect_write_line (&Out_Map, type, Points); 
         }  /* end lines section */

        Vect_close (&Out_Map); 
        Vect_close (&Map); 


        if (in!=NULL) {
          fprintf(stderr,"\nCreating Att file...\n");
          new_data = G_tempfile();
		     /* Open output "temp dig_att" file */
	  if ( (out1 = fopen (new_data, "w")) == NULL)
	   {
	      sprintf(buffb,"Can't create temporary output att file <%s> \n", 
                                               new_data) ;
	      G_fatal_error(buffb) ;
	   }
 
           while(fgets(buffa,80,in))
           {
	     if (strncmp(buffa,"A",1) == 0 ||
	       strncmp(buffa,"L",1) == 0 ||
	       strncmp(buffa,"P",1) == 0 )
		 {
                 sscanf(buffa,"%s %lf %lf %d", ctype, &X, &Y, &cat);
                 pj_do_proj(&X,&Y,&info_in,&info_out);
                          /* write line */
                  sprintf(buffb,"%1s %14.10f %14.10f %7d\n",ctype,X,Y,cat);
                  fputs(buffb,out1);
	          }
	      }
           fclose (in);
	   fclose (out1);
                     /* copy temp dig_att file to original */
	   sprintf(buffa, "cp %s %s/%s/dig_att/%s",
			     new_data,G_location_path(),oset_name,omap_name);
           if (system( buffa) )
	      {
	      sprintf(buffa,
               " Could not create file: '%s/dig_att/%s'"
			, iset_name,omap_name) ;
              fclose(out1);
              unlink(new_data);
	      G_fatal_error(buffa) ;
	      }
              unlink(new_data);
         }


		/* get the dig_cats file name*/
        if (access(cat_file, 0) == 0)
	   {
           fprintf(stderr,"\nCreating category file...\n");
                /* copy dig_cats file to this location */
	   sprintf(buffa,"cp %s %s/%s/dig_cats/%s",
			     cat_file,G_location_path(),oset_name,omap_name);

           if (system( buffa) )
	      {
	      fprintf(stderr,
               "WARNING(%s):  Could not create file: '%s/dig_cats/%s'\n"
			, argv[0], iset_name,omap_name) ;
	      }
	   }


fprintf(stderr, "\n\n%s of vector file <%s> has completed\n",
argv[0],map_name);
fprintf(stderr, "vector file <%s> in mapset <%s> will require\n",omap_name,oset_name);
fprintf(stderr, "  v.support be run, before the data is usable\n");

	return 0;
}







/* copy of Vect_open_new, modified to open map in another mapset */
int 
scs_open_new (struct Map_info *Map, char *name, char *mapset)
{
    char name_buf[200];
    int f_p;
    FILE *fp;

    Vect_init ();	/* init vector system */

/*DEBUG debugf ("Openning file %s level 1 WRITE\n", name);*/
/*  if (NULL == (fp = G_fopen_new ("dig", name)))*/

    G__file_name (name_buf, "dig", name, mapset);
    if (access(name_buf,0) != 0)
          close (creat (name_buf, 0666));
    if (NULL == (fp = fopen(name_buf, "w")))
          return -1;

    Map->dig_fp = fp;

    G__file_name (name_buf, "dig", name, mapset);
    Map->digit_file = G_store (name_buf); /*need?*/
    Map->open  = VECT_OPEN_CODE;
    Map->level = LEVEL_1;
    Map->mode  = MODE_WRITE;
    Map->name = G_store (name);
    Map->mapset = G_store (mapset);
    Map->Constraint_region_flag = 0;	/* these do not apply to to write, but*/
    Map->Constraint_type_flag   = 0;    /* init them anyway                   */

    Vect__init_head (&(Map->head));
    Vect__write_head_binary (Map, &(Map->head)); /* note dig_ => dig__*/

    return 1;
}

