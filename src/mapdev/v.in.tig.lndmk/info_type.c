/* Give the user lots of info, and ask if she wants point, area
   or both types of Landmark features.
Returns: sets results in feature_type array, and map names.
*/

#include "gis.h"
#include "site.h"
#include "vask.h"
#include "globals.h"

int ask_info_type (void)
{
int n, typ;
char exit_yes[2],view_yes[2];

typ = 0;

 while (typ!=1){
  *feature_type[0] = '\0';
  *feature_type[1] = '\0';
  *feature_type[2] = '\0';
  *feature_type[3] = '\0';
  *view_yes = '\0';
  *exit_yes = '\0';
  n = 1;
  V_clear();
  V_line(n++ ,"       GRASS IMPORT FROM CENSUS LANDMARK FEATURE RECORDS");
  V_line(++n ,"STEP 1:  Select Type(s) of features to be extracted");
  n++;
  V_line(++n ,"   There are Point and Area (polygon) Landmark feature types.");
  V_line(++n ,"     in the Landmark (Type 7) Records");
  n++;
  V_line(++n ,"   There may also be some Line (vector) Landmark features");
  V_line(++n ,"     in the Basic Data (Type 1) Records");
  n ++;
  V_line(++n ,"   Mark one feature type you wish extracted:");
  n ++;
  V_line(++n ,"                   Point");
  V_ques(feature_type[1],'s',n,27,1);
  V_line(++n ,"                   Area");
  V_ques(feature_type[2],'s',n,27,1);
  V_line(++n ,"                   Line");
  V_ques(feature_type[3],'s',n,27,1);
  n++;
  V_line(++n ,"   View CFCC Codes for selected type");
  V_ques(view_yes,'s',n,38,1);
  n++;
  V_line(++n ,"   or  Exit program");
  V_ques(exit_yes,'s',n,21,1);

  V_intrpt_ok();
  if (! V_call()) return (0);
  if (*exit_yes)  return (0);
  typ = 0;
  if (*feature_type[1]){*feature_type[1] = 'P'; typ++;}
  if (*feature_type[2]){*feature_type[2] = 'A'; typ++;}
  if (*feature_type[3]){*feature_type[3] = 'L'; typ++;}
  if (*view_yes && typ == 1) {
    if (show_list("") == -1) reset_cats();
    typ = 0;
  }
 } /* end of while */
/* now get file name(s) */
 *site_name = '\0'; *vect_name = '\0';
 if (*feature_type[1]=='P')
   mapset = G_ask_sites_new(
         "Input name of new site map for Landmark Points",site_name);
 if (*feature_type[2]=='A' || *feature_type[3]=='L')
   mapset = G_ask_vector_new(
         "Input name of new vector map for Landmark Features",vect_name);
 if (mapset==(char *)NULL || (*site_name=='\0' && *vect_name=='\0'))
   return -1;
 else
   return 1;
}

