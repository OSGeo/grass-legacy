#ifdef LATLON

#include	<stdio.h>
#include 	"projects.h"
#include        "gis.h"
#include        "digit.h"


extern Widget digbutton;
static struct pj_info      info_ll, info_coord;     /* Conversion info */


get_conv_info (coor_file_name)   
/* asks whether to register points in lat/lon; if yes, initializes
   proj structures */

   char *coor_file_name;

{
	char	buff[1024] ;
        char    buff1[200];
        char    x[20], y[20];
        int     temp;
        struct  Key_Value *proj_keys, *unit_keys;  
        FILE    *fp;
        int     nopt,i;
        char    *env_digitizer;
	int reg_ll_flag;  /* if reg file says is ll */
	int    proj_file_flag = 0; /* 1 if no proj_info file */
	int    reg_file_flag = 0; /* 1 if no reg_info file */

    reg_ll_flag = 0;  
    ll_flag = 0;
    proj_file_flag = 0;

      if( (fp = fopen (coor_file_name, "r"))  != NULL)
      {
	reg_file_flag = 1;

        /* try to figure out from coord_file if registration should be
            performed in  lat/lon */ 
        /* check if file was created by beta version (coord coord flag) */
         if (fgets(buff1,sizeof(buff1),fp)!=NULL) 
	 {
           if (sscanf(buff1,"%s %s %d",x,y,&temp) != 3) 
	   {
           /* then file was created by 4.1 version (coord coord) */
           /* in this case assume not lat/lon registration */
             if (sscanf(buff1,"%s %s",x,y) == 2) 
	     {  
               reg_ll_flag = 0;
             }
             else
               G_fatal_error("reg file format error");
           }
           else 
	   {        /* for 4.1 beta version */ 
             reg_ll_flag = temp;
           }
         }
         fclose (fp) ;
       }
       else
	reg_file_flag = 0;

      /* if (reg_ll_flag) */   /* there is a reg file and it is in lat/lon */
      {
          proj_keys = G_get_projinfo();
          unit_keys = G_get_projunits();
           
	  proj_file_flag = (proj_keys) && (unit_keys);



	  if (reg_file_flag && !proj_file_flag)
	  {
		ll_flag = 0;
		if (reg_ll_flag)
		{
		    make_monolog (1, "Projection info is not set. Digitizer will be disabled");
		    Dig_Enabled = 0;
		    dig_on_off();
		    XtSetSensitive (digbutton, False);

		    return;
		}
	    goto done;
	  }


	  if (proj_file_flag && !reg_file_flag)
	  {
	      sprintf(buff,"Do you want to register map coordinates in latitude/longtitude?");
	      ll_flag = mouse_yes_no (buff);
	    goto done;
	  }

	  ll_flag = reg_file_flag && proj_file_flag;
      }

done:

      if (ll_flag) 
      {
        if (pj_get_string(&info_ll,"+proj=ll") <0)
          G_fatal_error("Could not initialize proj_ll");

        if (pj_get_kv(&info_coord,proj_keys,unit_keys) <0) 
          G_fatal_error("Could not initialize proj_coord");
      }
}


int do_conversion(x,y,ll_coord)   /* if coord=1 then convert lat/lon
        to coord, otherwise convert coord to lat/lon */

double *x,*y;
int    ll_coord;       /* equals to 1 when conversion is ll to coord */
                       /* and 0 otherwise */
{
 if (ll_coord == 1) {
   if (pj_do_proj(x,y,&info_ll, &info_coord) <0) return -1;
   else return 1;
 }
 else {
   if (pj_do_proj(x,y,&info_coord, &info_ll) <0) return -1;
   else return 1;
 }
}

#endif	/* LATLON */
