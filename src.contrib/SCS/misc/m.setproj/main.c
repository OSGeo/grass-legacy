/* %W% %G% */
/* main.c    1.1   05/16/91  GRASS4.0
*    Created by : M.L.Holko , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of creating a new projection
*             information file
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             m.setproj set=mapset for output project info file
*                    proj=projection of the output project info file
*
*/

#include <stdio.h>
#include <ctype.h>
#include  "gis.h"
#define MAIN

#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"


main (argc,argv) 
int argc; char *argv[]; 
{ 
	int Out_proj, out_zone, out_stat, ret, i; 
	char cmnd2[500], out_lon0[20], out_lat0[20];
	char proj_out[20], proj_name[50], set_name[20], *value1;
	char path[256],buffa[256],buffb[256],answer[50];
        struct Option *projopt, *setopt;
        struct Key_Value *out_proj_keys;
	FILE *ls, *out1, *FPROJ;
	char *new_data, *key, *value, spheriod[50];
	int j, k;

        G_gisinit (argv[0]);
     
		 /* set up the options and flags for the command line parser */

        setopt = G_define_option();
        setopt->key             =  "set";
        setopt->type            =  TYPE_STRING;
        setopt->required        =  NO;
        setopt->description     =  "mapset to place the projection info file";
 
        projopt = G_define_option();
        projopt->key             = "proj";
        projopt->type            =  TYPE_STRING;
        projopt->required        =  YES;
        projopt->options         = "utm,aea,stp,ll,lcc,merc,tmerc,xxx";
        projopt->description     = "projection name";

 
	   /* heeeerrrrrre's the   PARSER */
        if (G_parser (argc, argv))
		    exit (-1);
		     
           /* start checking options and flags */

	if (setopt->answer)
            sprintf(set_name,"%s",setopt->answer);
	else
            sprintf(set_name,"%s",G_mapset());
        sprintf(path,"%s/%s",G_location_path(),set_name);

	     /* get the output projection parameters, if existing */
        G__file_name (path,"",PROJECTION_FILE,set_name);
        if (access(path,0) == 0)
	   {
           sprintf(buffb,
	   "projection file PROJ_INFO exists \n");
                G_fatal_error(buffb) ;
           }
	else 
	   {
           out_stat = -1;
	   if (projopt->answer != NULL)
	      {
	      sprintf(proj_out,"%s",projopt->answer);
              Out_proj = 0;
              if ((strncmp(proj_out,"UTM",3) == 0) ||
                  (strncmp(proj_out,"utm",3) == 0))   
	          {
	          Out_proj = 1;
	          sprintf(proj_name,"Universal Transverse Mercator");
	          }
              if ((strncmp(proj_out,"STP",3) == 0) ||
                  (strncmp(proj_out,"stp",3) == 0))   
	          {
	          Out_proj = 2;
	          sprintf(proj_name,"State Plane");
	          }
              if ((strncmp(proj_out,"LL",3) == 0) ||
                  (strncmp(proj_out,"ll",3) == 0))    
	          {
	          Out_proj = 3;
	          sprintf(proj_name,"Lat/Long");
	          }
              if ((strncmp(proj_out,"AEA",3) == 0) ||
                  (strncmp(proj_out,"aea",3) == 0))   
	          {
	          Out_proj = 4;
	          sprintf(proj_name,"Albers Equal Area");
	          }
              if ((strncmp(proj_out,"LCC",3) == 0) ||
                  (strncmp(proj_out,"lcc",3) == 0))   
	          {
	          Out_proj = 5;
	          sprintf(proj_name,"Lambert Conformal Conic");
	          }
              if ((strncmp(proj_out,"MER",3) == 0) ||
                  (strncmp(proj_out,"mer",3) == 0))   
	          {
	          Out_proj = 6;
	          sprintf(proj_name,"Mercator");
	          }
              if ((strncmp(proj_out,"TME",3) == 0) ||
                  (strncmp(proj_out,"tme",3) == 0))   
	          {
	          Out_proj = 7;
	          sprintf(proj_name,"Transverse Mercator");
	          }
              if ((strncmp(proj_out,"XXX",3) == 0) ||
                  (strncmp(proj_out,"xxx",3) == 0))   
	          {
		  Out_proj = 99;
	          sprintf(proj_name,"Special Mapgen Projection");
	          }
              if (Out_proj == 0)
                 {
                 fprintf(stderr,"Unknown projection\n\tKnown Projections:\n");
                 fprintf(stderr,"[UTM] or [utm] Universal Tranverse Mercator\n");   
                 fprintf(stderr,"[STP] or [stp] State Plane\n");
                 fprintf(stderr,"[LL]  or [ll]  Lat/Long\n");
                 fprintf(stderr,"[AEA] or [aea] Albers Equal Area\n");
                 fprintf(stderr,"[LCC] or [lcc] Lambert Conformal Conic\n");
                 fprintf(stderr,"[MERC] or [merc] Mercator\n");
                 fprintf(stderr,"[TMERC] or [tmerc] Transverse Mercator\n");
                 exit(0);
                 }
	      }
	   else
	      {
              sprintf(buffb,"No output projection option selected\n");
              G_fatal_error(buffb) ;
              }
	   }


/*****************   GET spheriod  **************************/
           if (Out_proj != 3) 
	       {
	       if (!get_ellp(spheriod))
	           {
	           sprintf(buffb,"An output spheriod is required\n");
	           G_fatal_error(buffb) ;
		   }
	       }
/*** END get spheriod  ***/

           if (Out_proj == 1)
             {
             sprintf(cmnd2, "+proj=%s\t", proj_out);
             fprintf(stderr,"\n\tENTER utm zone : ");
             gets(answer);
             if (strlen(answer) == 0) 
	           {
	           sprintf(buffb,"An output utm zone is required\n");
	           G_fatal_error(buffb) ;
		   }
             else out_zone = atoi(answer);
	     sprintf(buffb,"\t+zone=%d",out_zone);
             strcat(cmnd2, buffb);
             }
           if (Out_proj == 2)
	     {
	     	get_stp_proj(buffb);
			for (i=0; i < strlen(buffb); i++) 
				if (buffb[i] == ' ') {buffb[i] = '\t';}
             sprintf(cmnd2,"%s\t\n",buffb);

	     }
	   if (Out_proj == 3) sprintf(cmnd2,"+proj=ll");
           if (Out_proj >= 4 && Out_proj <= 7)
	     {
	     out_zone = i = 0;
	     while (i <= 1)
	       {
	       G_clear_screen();
	       if (!i) fprintf(stderr,
              "\n Enter %s longitude for the central meridian [default 96w]: ",
		                         proj_out);
               else fprintf(stderr,
              "\n Enter %s latitude for the standard parallel [default 23n]: ",
					 proj_out);
               gets(answer);
               if (strlen(answer) == 0)
		  {
		  if (!i) sprintf(out_lon0,"96w");
                  else sprintf(out_lat0,"23n");
		  i++;
		  continue;
		  }
	       else if (!get_deg(answer,i)) continue;
	       else
		  {
		  if (!i) sprintf(out_lon0,"%s",answer);
                  else sprintf(out_lat0,"%s",answer);
		  i++;
		  continue;
		  }
	       }
	     sprintf(cmnd2,
		       "+proj=%s\t+lon_0=%s\t+lat_0=%s\t", 
				   proj_out, out_lon0, out_lat0);
	     }
           if (Out_proj == 99)
			{
			fprintf(stderr,"\tEnter mapgen proj parameters : ");
			gets(buffa);
			if (strlen(buffa) == 0)
				{
				sprintf(buffa,"A proj command is required, quitting");
				G_fatal_error(buffb) ;
				}
			/* read values and create input proj command buffer */
			for (i=0; i < strlen(buffa); i++) 
				if (buffa[i] == ' ') {buffa[i] = '\t';}
			}



	  /* create the PROJ_INFO & PROJ_UNITS files, if required */
	  /*********** Cann't seem to get keys to work abandon for now ***
        if (access(path,0) != 0)
	   {
	   out_proj_keys = G_create_key_value();
	   G_set_key_value ("name", proj_name, out_proj_keys);

		for (i = 0; i < strlen(cmnd2); i++) {
			j = k = 0;
			if (cmnd2[i] == '+') {
				while (cmnd2[++i] != '=') buffa[j++] = cmnd2[i];
				buffa[j] = 0;
				while (cmnd2[++i] != '\t' && cmnd2[i] != '\n' &&cmnd2[i] != 0) buffb[k++] = cmnd2[i];
				buffb[k] = 0;
		fprintf(stderr,"%s %s \n",buffa,buffb);
				G_set_key_value (buffa, buffb, out_proj_keys);
				}
		}
            G_write_key_value_file (path, out_proj_keys, &out_stat);
	    if (out_stat != 0)
	          {
	          sprintf(buffb,"Error writting PROJ_INFO file\n");
	          G_fatal_error(buffb) ;
	          }
	    G_free_key_value(out_proj_keys);
		************* End of attempt at keys *****do direct out put******/

	sprintf(buffa,"%s/%s",G_location_path(),set_name);
	if (access(buffa,0) != 0)
	   {
	   fprintf(stderr,
		 "\nMapset [%s] does NOT exist, creating new mapset\n",set_name);
	   sprintf(buffb,"mkdir %s\n",buffa);
	   system(buffb);
	   }
        if (access(path,0) != 0)
	   {
	   FPROJ = fopen (path,"w");
	   if ( (FPROJ = fopen(path, "w")) == NULL)
	      G_fatal_error("Opening new PROJ_INFO file");

	   fprintf(FPROJ,"name: %s\n", proj_name);

           if (Out_proj != 3) 
	   		fprintf(FPROJ,"ellps: %s\n", spheriod);

		for (i = 0; i < strlen(cmnd2); i++) {
			j = k = 0;
			if (cmnd2[i] == '+') {
				while (cmnd2[++i] != '=') buffa[j++] = cmnd2[i];
				buffa[j] = 0;
				while (cmnd2[++i] != '\t' && cmnd2[i] != '\n' &&cmnd2[i] != 0) buffb[k++] = cmnd2[i];
				buffb[k] = 0;
				fprintf(FPROJ,"%s: %s \n",buffa,buffb);
				}
		}
	   fclose(FPROJ);
	   }

        G__file_name (path,"",UNIT_FILE,set_name);
        if (access(path,0) != 0)
	   {
	   if ( (out1 = fopen (path, "w")) == NULL)
	      {
	      sprintf(buffb,"Can't create UNITS output file\n") ;
	      G_fatal_error(buffb) ;
	      }
          
	   if (Out_proj == 2)
	      sprintf(buffb,"unit:foot\nfoot:.3048\n");
	   else if (Out_proj == 3)
	      sprintf(buffb,"unit:degree\ndegree:1.0\n");
           else
	      sprintf(buffb,"unit:meter\nmeter:1.0\n");
           fputs(buffb,out1);
	   fclose (out1);
	   }
}
