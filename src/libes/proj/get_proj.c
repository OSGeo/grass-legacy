#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include "projects.h"
#include  "gis.h"
#define MAIN

#define PERMANENT "PERMANENT"
#define MAX_PARGS 100

int pj_get_kv (info,in_proj_keys,in_units_keys)
  struct pj_info *info; 
  struct Key_Value *in_proj_keys, *in_units_keys;

{
        char *opt_in[MAX_PARGS];
        char s[20]; 
        char *str;
        int  nopt, nopt1;
	int i,k;
        int nsize;
        char buffa[300], zonebuff[50], buffb[300], factbuff[50];
	char proj_in[50];
	char ipath[256], opath[256];
        PJ *pj;

         info->zone = 0;
         info->meters = 1.0;
         info->proj[0] = '\0' ;

         str = G_find_key_value("meters", in_units_keys);
	 if(str != NULL)
	 {
           strcpy(factbuff, str);
           if(strlen(factbuff)>0)
             sscanf(factbuff,"%lf", &(info->meters));
         }

         str = G_find_key_value("name", in_proj_keys);
	 if(str != NULL)
	 {
            sprintf(proj_in,"%s", str);
         }
         str = G_find_key_value("proj", in_proj_keys);
	 if(str != NULL)
	 {
            sprintf(info->proj,"%s", str);
         }
         str = G_find_key_value("zone",in_proj_keys);
         if (str != NULL) 
	 {
           if(sscanf(str,"%d", &(info->zone))!=1) 
	   {
             sprintf(buffa,"Invalid zone %s specified", str);
             G_fatal_error(buffa);
           }
         }

         if(strlen(info->proj)<=0) 
                      sprintf(info->proj,"ll"); 

         if (strncmp(proj_in,"Lat",3) == 0) {
           return 1; 
         }

	 nopt = in_proj_keys->nitems-1;
         nopt1=nopt;
         k=0;
	 for (i=1; i<=nopt; i++)
         {
	   if(strncmp(in_proj_keys->key[i],"south",5)==0)
	     sprintf(buffa,"south");
           else {
	     sprintf(buffa,"%s=%s",
	         in_proj_keys->key[i],in_proj_keys->value[i]);
           }
	   if((strncmp(in_proj_keys->key[i],"ellps",5)==0) &&
	      (strncmp(in_proj_keys->value[i],"sphere",6)!=0))
           nopt1--;
           else {
             nsize=strlen(buffa);
             if(!(opt_in[k] = (char *)malloc(nsize+1)))  {
                fprintf(stderr,"cannot allocate options\n");
                exit(0);
             }
             sprintf(opt_in[k],buffa);
             k++;
           }
         }

         if (strncmp(proj_in,"Lat",3) != 0)  {
            if 	(!(pj = pj_init(nopt1,opt_in))) {
              fprintf(stderr,"cannot initializa pj\n");
              exit(0);
            }
            info->pj = pj;
         } 
       

         return 1;

}
         


int pj_get_string (info,str)
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
         info->meters = 1.0;

         if ((str == NULL) || (str[0] == '\0')) {
           sprintf(info->proj,"ll"); 
           return 1;
         }
         s = str;
	 while (s = strtok(s," \t\n"))  
         {
           if (strncmp(s,"+unfact=",8)==0) {
             s = s+8;
             info->meters = atof(s);
           }
           else {
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
         

int pj_zero_proj (info)
  struct pj_info *info; 
{

         info->zone = 0;
         info->proj[0] = '\0' ;
         info->meters = 1.0;
}


