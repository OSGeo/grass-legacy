
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include "projects.h"
#include  "gis.h"
#include  "pjinf.h"

/*
#define MAIN
*/

#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"

#define PERMANENT "PERMANENT"
#define MAX_PARGS 100

int G_get_proj_PROJINFO (info)
  struct pj_info *info; 

{
        char *opt_in[MAX_PARGS];
        char s[20];
        int  nopt;
	int i, in_stat, in_stat_ell;
        int nsize;
        char buffa[300], zonebuff[50], buffb[300];
	char proj_in[50];
	char ipath[256];
        struct Key_Value *in_proj_keys;
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

	 nopt = in_proj_keys->nitems-1;
	 for (i=1; i<=nopt; i++)
         {
	   sprintf(buffa,"%s=%s",
	         in_proj_keys->key[i],in_proj_keys->value[i]);
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

         return 1;

}
         


int G_get_proj_string (info,str)
  struct pj_info *info; 
  char   *str;
{
        char *opt_in[MAX_PARGS];
        char *s;
        int  nopt=0;
	int i;
        int nsize;
        char buffa[300], zonebuff[50], buffb[300];
	char proj_in[50];
        PJ *pj;

         info->zone = 0;
         info->proj[0] = '\0' ;
         info->stp = 0;

         if ((str == NULL) || (str[0] == '\0')) {
           sprintf(info->proj,"ll"); 
           return 1;
         }
         s = str;
	 while (s = strtok(s," \t\n"))  
         {
           if (strncmp(s,"+",1)==0) ++s;
           if (nsize = strlen(s)) {
             if (nopt >= MAX_PARGS) {
     fprintf(stderr,"nopt = %d, s=%s\n",nopt,str); 
               G_fatal_error("Option input overflowed option table");
             }
             if(!(opt_in[nopt] = (char *)malloc(nsize+1)))  
               G_fatal_error("Option input memory failure");
             sprintf(opt_in[nopt++],s);

             if (strncmp("proj=",s,5) == 0) {
               sprintf(info->proj,"%s",s+5);
               if (strncmp(info->proj,"stp",3) == 0) info->stp = 1;
               if (strncmp(info->proj,"ll",2) == 0) {
                 sprintf(info->proj,"ll"); 
                 return 1;
               }	
             }
             if (strncmp("zone=",s,5) == 0) {
               sprintf(zonebuff,"%s",s+5);
               sscanf(zonebuff,"%d", &(info->zone));
             }
           
           }
           s = 0;
         }

         if (strncmp(info->proj,"ll",2) != 0)  {
            if 	(!(pj = pj_init(nopt,opt_in))) {
              fprintf(stderr,"cannot initializa pj\n");
              exit(0);
            }
            info->pj = pj;
         } 
         return 1;
}
         

int G_zero_proj (info)
  struct pj_info *info; 
{

         info->zone = 0;
         info->proj[0] = '\0' ;
         info->stp = 0;
}


