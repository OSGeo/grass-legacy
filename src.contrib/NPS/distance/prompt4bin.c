#include "distance.h"
#include <sys/types.h>
/* It should be noted that both "gis.h" file and "sys/types.h" file both    */
/* have defined the same variable named "uchar".  This is alright since     */
/* both of the definitions are exactly the same.  The definitions are:      */
/* typedef unsigned char uchar                                              */
#include <sys/stat.h>
int
prompt4bin(bin_name)
 char *bin_name;
 {
  char s[20];
  char user_name[20];
  char info_line1[81];
  char info_line2[81];
  char msg_line[81];
  struct stat stbuf;
  char full_name[512];
  int dup;

  dup = 0;
  while(1)
   {
    V_clear();
    if (dup)
     {
      sprintf(msg_line,"File: \"%s\" already exists.  Please select a different name.",s);
      V_line(3,msg_line);
     }
    strcpy(s,"");
    sprintf(info_line1,"File will be created in directory:                                             ");
    sprintf(info_line2,"\"%s/%s/%s/dig\"",
            G_gisdbase(),G_location(),G_mapset() );
    V_line(1,"                                    d.distance");
    V_line(5,"Please provide a file name for the digit binary file to be created:");
    V_ques(s,'s',7,1,19);
    V_line(9,info_line1);
    V_line(10,info_line2);
    V_intrpt_ok();
    if (!V_call())
      return(0);
    sprintf(full_name,"%s/%s/%s/dig/%s",
            G_gisdbase(),G_location(),G_mapset(),s);
    if (stat(full_name,&stbuf)==0)
     {
      dup = 1;
     }
    else
     {
      strcpy(bin_name,s);
      return(1);
     }
   }
 }
