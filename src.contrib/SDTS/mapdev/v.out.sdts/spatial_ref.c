#include "globals2.h"
#include "gis.h"


#define LL_SFAX -0.000001
#define LL_SFAY  0.000001
#define UTM_SFAX 0.01
#define UTM_SFAY 0.01


init_SDTS_proj_info (Map)
   struct Map_info *Map;
{
    int proj, in_stat;
	char ipath[1024], *str;
	struct Key_Value *proj_keys;
	static char *PERMANENT = "PERMANENT";

    /*get spheroid name, if exists in PROJ_INFO  file*/
    G__file_name (ipath, "", PROJECTION_FILE, PERMANENT);
    if (access(ipath,0) ==0)
    {
		proj_keys = G_read_key_value_file(ipath, &in_stat);
		if (in_stat == 0)
		   if ((str = G_find_key_value ("ellps", proj_keys)) != NULL)
				strcpy (AP00_ellps, str);
    }

	proj = G_projection();

	switch (proj)
	{
		case 1: /*UTM*/
		  Sfax = UTM_SFAX;
		  Sfay = UTM_SFAY;
		  strcpy (Xref_rsnm, "UTM");
		  strcpy (Iref_Xlbl, "EASTING");
		  strcpy (Iref_Ylbl, "NORTHING");
	      sprintf (Zone_str, "%d", G_zone ());
		  break;
        case 2: /*state-plane*/
		  G_fatal_error 
			 ("State Plane not supported yet by GRASS-SDTS software\n");
		  break;
        case 3: /*lat-lon*/
		  Sfax = LL_SFAX;
		  Sfay = LL_SFAY;
		  strcpy (Xref_rsnm, "GEO");
		  strcpy (Iref_Xlbl, "LONGITUDE");
		  strcpy (Iref_Ylbl, "LATITUDE");
		  /*null zone string for xref module*/
	      Zone_str[0] = '\0';
		  break;
        default:
		  G_fatal_error ("Unknown or unsupported projection\n");
		  break;
     }
  
	 /*use reciprocal of SDTS scale factors for export data conversion*/
     Sfax_out = 1.0 / Sfax;
	 Sfay_out = 1.0 / Sfay;
}
