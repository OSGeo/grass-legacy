 /*  @(#)init_map.c	2.1  6/26/87  */

/**
*	There are two times init_map() will be called.  Once when the user first
*	sets up the map setup_map(),   and when the user wants to reset	the map
*	reset_map() .
*
**/


#include        "gis.h"
#include        "digit.h"
#include	"map.h"

#include	<stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>


reset_map (w,  map, coor_file)
	Widget w;
	struct Map_info *map;
	char	*coor_file ;
{
    int	n_points, i, j;
    int	status;
    char	buff[85] ;
#ifdef LATLON
        char    temp[25];
        double  X[MAX_COOR],Y[MAX_COOR];
        int ask_again;
#endif

	FILE	*fp,	*fopen () ;
	int ok;

    ok = 0;
    while (!ok)
    {

    /*  initiliaze use[],  no points valid  */
    for (i=0 ;  i<MAX_COOR ;  ++i)
    {
	*(use+i) = 0 ;  	*(bx+i) = 0 ;  	*(by+i) = 0 ;
	*(residuals+i) = 0 ; 
#ifdef LATLON
        bcx[i][0] ='\0';
        bcy[i][0] ='\0';
#endif
    }
    
    n_points = 0 ;
    /*  if the coordinate file isn't there it means we ask for um  */

    if( (fp = fopen (coor_file, "r"))  != NULL)
    {
#ifndef LATLON 
        if(ask_driver_yes_no ("Use set of registered points from last session?")) 
#endif
	    n_points = load_coor_from_file (fp);
	fclose (fp) ;
    }

    reg_cnt = 0 ;

while (1)
 {

#ifdef LATLON   
        if (ll_flag) {
          do {   /* keep reasking until all entered strings are valid */
            ask_again = 0;
            if ((n_points =  ask_map_coor_ll ()) < 0)
		return(-1) ;

            for (j=0; j<n_points; j++) {
              if ((bcx[j][0]!='\0') || (bcy[j][0]!='\0')) {
                if (G_scan_northing (bcx[j],Y+j,PROJECTION_LL) == 0)
                {
                   fprintf(stderr,"Invalid longtitude: %s\n",bcx[j]);
                   ask_again = 1;
                   bcx[j][0] = '\0'; /* wipe out incorrect entry */
                }
                if (G_scan_easting (bcy[j],X+j,PROJECTION_LL) == 0)
                {
                   fprintf(stderr,"Invalid latitude: %s\n",bcy[j]);
                   ask_again = 1;
                   bcy[j][0] = '\0'; /* wipe out incorrect entry */
                }
              } 
            }
            if (ask_again == 1) { 
              do {
                fprintf(stderr,"Press <Return> to continue ");
              } while (!G_gets(buff));
            }
/* Make sure nonregistered fields are empty */
	    for (i=n_points ;  i<MAX_COOR ;  ++i) {
              bcx[j][0] = '\0';
              bcy[j][0] = '\0';
            }
          } while (ask_again == 1);

          /* at this point X and Y contain registered points */
          for (j=0; j<n_points; j++) {
            if (ll_flag) {  /* if they are in lat/lon convert */
              if (do_conversion(X+j,Y+j,1) < 0) {
                sprintf(buff,"Could not convert %s %s to coord",bcx[j],bcy[j]);
                fprintf(stderr,buff);
                close_down(0);
              }
            }
            bx[j] = X[j];
            by[j] = Y[j];
          }
        }  /* if ll_flag */
        else
#endif
        if ((n_points =  ask_map_coor ()) < 0)
                return(-1) ;

        status =  register_map_coor(n_points) ;
        if (status < 0)
                return (-1) ;
        if (status == 1)
	{
	    save_coor (coor_file);
            break ;
	}

    }              /*  while (1)   */
    check_map(map);
    /*CHANGE to ask_yes_no when added */

    ok = ask_driver_yes_no ("Satisfied with registration ?");
    }
    /*while (!ok)*/
    
    return(0) ;

}


save_coor (coor_file)
    char    *coor_file ;
{
    FILE *fp;

    if ( (fp = fopen (coor_file, "w"))  != NULL)
    {
	save_coor_to_file (fp) ;
	fclose (fp) ;
    }

    return(0) ;

}
