#define MAIN

#include <stdio.h>
#include "gis.h"
#include "gtoa.h"
#include "digit.h"
#include "dig_head.h"

struct Map_info Map;

main()
{
int	done=0;
char	cov_type[1000],
    	prefix[1000],
    	msg[1000],
    	*mapset,
    	name[1000],
    	dig_filepath[1000],
        dig_filename[1000],
    	att_filename[1000],
    	cat_filename[1000],
    	lin_filename[1000],
    	pol_filename[1000],
    	lab_filename[1000],
    	txt_filename[1000];
FILE	*dig_fp,
        *pol_file,
	*lin_file,
	*lab_file,
	*txt_file;

pol_flg = 0;
lin_flg = 0;
lab_flg = 0;
txt_flg = 0;

G_gisinit("grass.to.arc");

do {
   printf("\n COVERAGE TYPE\n");
   printf("Enter \"polygon\" or \"line\"\n");
   printf("Hit RETURN to cancel request\n");
   printf("> ");
   gets(cov_type);

   /* EXIT IF USER HIT RETURN */

   if (strcmp(cov_type,"") == 0)
      exit(0);
   }
while (strcmp(cov_type,"polygon")!=0 && strcmp(cov_type,"line")!=0);

mapset = G_ask_old(" BINARY VECTOR (DIGIT) FILE TO CONVERT ",name,
                   "dig","binary vector");
if (!mapset) exit(0);

dig_P_init(name,mapset,&Map);
if (!Map.all_areas || !Map.all_isles)
   {
   G_fatal_error("Please run support.vect");
   exit(-1);
   }

done=0;
do {
   printf("\n ARC/INFO GENERATE FORMAT FILENAME PREFIX \n");
   printf("Enter a filename prefix to be used in the creation\n");
   printf("of ARC/INFO Generate format files\n");
   printf("Hit RETURN to cancel request\n");
   printf("> ");
   gets(prefix);
   if (strcmp(prefix,"")==0)
      exit(0);
   else
      {
      strcpy(pol_filename,prefix);
      strcpy(lin_filename,prefix);
      strcpy(lab_filename,prefix);
      strcpy(txt_filename,prefix);
      strcat(pol_filename,".pol");
      strcat(lin_filename,".lin");
      strcat(lab_filename,".lab");
      strcat(txt_filename,".txt");

      if ( fopen(pol_filename,"r") != NULL ||
           fopen(lin_filename,"r") != NULL ||
           fopen(lab_filename,"r") != NULL ||
           fopen(txt_filename,"r") != NULL )
         {
         sprintf(msg,"File(s) with the prefix %s already exist(s)\n",prefix);
         G_warning(msg);
         }
      else
         {
         done=1;
         pol_file = fopen(pol_filename,"w");
         lin_file = fopen(lin_filename,"w");
         lab_file = fopen(lab_filename,"w");
         txt_file = fopen(txt_filename,"w");
         }
      }
   }
while (!done);

if (strcmp(cov_type,"polygon")==0)
   {
   write_areas(name,mapset,&Map,lin_file,lab_file,txt_file);
   dig_P_fini(&Map);
   G__file_name(dig_filepath,"dig",name,mapset);
   dig_fp = fopen(dig_filepath,"r");
   dig_read_head_binary(dig_fp,&head);
   write_area_lines(dig_fp,lin_file);
   lin_flg=1;
   } 
else
   {
   write_lines(name,mapset,&Map,lin_file,txt_file);
   dig_P_fini(&Map);
   }
 
/* delete empty files */
if (!pol_flg)
   {
   strcpy(msg,"rm ");
   strcat(msg,pol_filename);
   system(msg);
   }
if (!lin_flg) 
   {
   strcpy(msg,"rm ");
   strcat(msg,lin_filename);
   system(msg);
   }
if (!lab_flg) 
   {
   strcpy(msg,"rm ");
   strcat(msg,lab_filename);
   system(msg);
   }
if (!txt_flg) 
   {
   strcpy(msg,"rm ");
   strcat(msg,txt_filename);
   system(msg);
   }


exit(0);
}
