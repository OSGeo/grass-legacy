#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <projects.h>
#include  "gis.h"
#include  "pjinf.h"
#define MAIN
/*
#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"
*/
#define PERMANENT "PERMANENT"
#define MAX_PARGS 100


int G_get_proj (info)
  struct pj_info *info; 

{
        char *opt_in[MAX_PARGS];
        char s[20];
        int  nopt;
	int i, in_stat, in_stat_ell;
        int nsize;
        char buffa[300], zonebuff[50], buffb[300];
	char proj_in[50];
	char ellips_in[50];
	char ellips1[50];
	char ipath[256];
        struct Key_Value *in_proj_keys, *ell_lookup_keys;
        PJ *pj;

         info->zone = 0;
         info->proj[0] = '\0' ;
         G__file_name (ipath, "", PROJECTION_FILE, PERMANENT);
         while (access(ipath,0) != 0)
         {
           fprintf(stderr,"file %s\n",ipath);
           fprintf(stderr,"PROJ_INFO file not found  for location %s\n",
	                					 G_location());
           return -2;
         }
         in_proj_keys = G_read_key_value_file(ipath,&in_stat);
         if (in_stat != 0)
         { 
             sprintf(buffa,"ERROR in reading current location PROJ_INFO\n");
             return -3;
         }
         sprintf(proj_in,"%s", G_find_key_value("name",in_proj_keys));
         sprintf(info->proj,"%s", G_find_key_value("proj",in_proj_keys));
         sprintf(zonebuff,"%s", G_find_key_value("zone",in_proj_keys));
         sscanf(zonebuff,"%d", &(info->zone));
         if(strlen(info->proj)<=0) 
                      sprintf(info->proj,"ll"); 
         if (strcmp(proj_in,"State Plane") == 0) info->stp=1;
           else info->stp=0;

         if (strncmp(proj_in,"Lat",3) == 0) {
           return 1; 
         }
         sprintf(ipath,"%s/etc/ellips.lookup",G_gisbase());
         while (access(ipath,0) !=0)
          {
            sprintf(buffb,"%s not found",ipath);
            G_fatal_error(buffb);
          }
         ell_lookup_keys = G_read_key_value_file(ipath,&in_stat_ell);
         if (in_stat != 0)
          {
            sprintf(buffb,"ERROR in reading %s",ipath);
            G_fatal_error(buffb);
          }
         sprintf(ellips1,"%s", G_find_key_value("ellps",in_proj_keys));
         sprintf(ellips_in,"%s", G_find_key_value(ellips_in,ell_lookup_keys));

	 nopt = in_proj_keys->nitems-1;
	 for (i=1; i<=nopt; i++)
         {
           sprintf(buffb,"%s",in_proj_keys->key[i]);
           G_strip(buffb);
           if (strcmp(buffb,"ellps")) {
	     sprintf(buffa,"%s=%s",buffb,ellips_in);
           }
           else {
	     sprintf(buffa,"%s=%s",
	         in_proj_keys->key[i],in_proj_keys->value[i]);
           }
           nsize=strlen(buffa);
           if(!(opt_in[i-1] = (char *)malloc(nsize+1)))  {
              fprintf(stderr,"cannot allocate options\n");
              exit(0);
           }
           sprintf(opt_in[i-1],buffa);
         }

         if (strncmp(proj_in,"Lat",3) != 0)  {
            if 	(!(pj = pj_init(nopt,opt_in))) {
              fprintf(stderr,"cannot initializa pj\n");
              exit(0);
            }
            info->pj = pj;
         } 
       

	 G_free_key_value(in_proj_keys);
	 G_free_key_value(ell_lookup_keys);


         return 1;

}
         

