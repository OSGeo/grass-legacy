
/*  @(#)init_map.c	2.1  6/26/87  */

/**
*	There are two times init_map() will be called.  Once when the user first
*	sets up the map setup_map(),   and when the user wants to reset	the map
*	reset_map() .
*
**/

#include	<stdio.h>
#include	"map.h"
#include        "gis.h"
#include        "digit.h"

#include        "projects.h"

init_map ( coor_file)
	char	*coor_file ;
{
	int	i, j ;
	int	status ;
	int	n_points;
        char    temp[25];
        double  X[MAX_COOR],Y[MAX_COOR];
	char	buff[85] ;
#ifdef LATLON
        struct  Key_Value *proj_keys, *unit_keys;
        static struct pj_info   info_ll, info_coord;    /* Conversion info */
        int ask_again;
#endif

	FILE	*fp,	*fopen () ;
        
	/*  initiliaze use[],  no points valid  */
	for (i=0 ;  i<MAX_COOR ;  ++i)
	 {
		*(use+i) = 0 ;  	*(bx+i) = 0 ;  	*(by+i) = 0 ;
#ifdef LATLON
		bcx[i][0] = '\0' ;  	
		bcy[i][0] = '\0' ;  	
#endif
		*(residuals+i) = 0 ;
	 }

	Write_info(1, "DIGITIZER SETUP") ;

	Clear_info() ;

	n_points = 0 ;

	/*  if the coordinate file isn't there it means we ask for um  */

	if( (fp = fopen (coor_file, "r"))  != NULL)
	 {
		n_points = load_coor_from_file (fp) ;

		fclose (fp) ;
		Write_info(2, "") ;
		Clear_info() ;
	  }

	reg_cnt = 0 ;

#ifdef LATLON
        if (n_points > 0) {
          if (ll_flag) {  /* if reg file exists and has lat/lon strings in it */
            proj_keys = G_get_projinfo();
            if (proj_keys == NULL) { 
            /* projection file had to be deleted after last registration */   
              fprintf(stderr,"projection file not found -- run g.setproj\n");
              last_words(CM,-1);
            }
            unit_keys = G_get_projunits();
            if (unit_keys == NULL)  {
            /* units file had to be deleted after last registration */   
              fprintf(stderr,"units file not found -- run g.setproj\n");
              last_words(CM,-1);
            }
            if (pj_get_string(&info_ll,"+proj=ll") <0) {
              fprintf(stderr,"Could not initialize proj_ll\n");
              last_words(CM,-1);
            } 
            if (pj_get_kv(&info_coord,proj_keys,unit_keys) <0)  {
            /* projection file had to be corrupted after last registration */   
              fprintf(stderr,"Could not initialize proj_coord\n");
              last_words(CM,-1);
            }
            ll_ask = 2;  /* user can enter only lat/lon strings */
          }
          else ll_ask = 0;
        }
        else {
         while(1) {
            proj_keys = G_get_projinfo();
            if (proj_keys == NULL)   {
              ll_ask = 0;  /* don't allow lat/lon registration */ 
              break;
            }
            unit_keys = G_get_projunits();
            if (unit_keys == NULL)  {
              ll_ask = 0;  /* don't allow lat/lon registration */ 
              break;
            }
            if (pj_get_string(&info_ll,"+proj=ll") <0) {
              ll_ask = 0;  /* don't allow lat/lon registration */ 
              break;
            }
            if (pj_get_kv(&info_coord,proj_keys,unit_keys) <0)  {
              ll_ask = 0;  /* don't allow lat/lon registration */ 
              break;
            }
            ll_ask = 1;
            break;
          }
        }
#endif


while (1)
 {
	/*  go to Vask page to enter the coordinates  */
	/*DEBUG*/ 
	debugf ("entering ask_map_coords\n");

#ifdef LATLON   
        if (ll_ask >0) {
          do {   /* keep reasking until all entered strings are valid */
            ask_again = 0;
            if ((n_points =  ask_map_coor_ll (n_points)) < 0)
		return(-1) ;

            for (j=0; j<n_points; j++) {
              if ((bcx[j][0]!='\0') || (bcy[j][0]!='\0')) {
                if (ll_flag) {
                  if (G_scan_northing (bcx[j],Y+j,PROJECTION_LL) == 0)
                  {
                     fprintf(stderr,"Invalid longtitude: %s\n",bcx[j]);
                     ask_again = 1;
                     ll_ask = 2;
                     bcx[j][0] = '\0'; /* wipe out incorrect entry */
                  }
                  if (G_scan_easting (bcy[j],X+j,PROJECTION_LL) == 0)
                  {
                     fprintf(stderr,"Invalid latitude: %s\n",bcy[j]);
                     ask_again = 1;
                     ll_ask = 2;
                     bcy[j][0] = '\0'; /* wipe out incorrect entry */
                  }
                }
                else {
                  if (!sscanf(bcx[j],"%lf",&X[j])) {
                    fprintf(stderr,"Invalid Easting : %s\n",bcx[j]);
                    ask_again = 1;
                    bcx[j][0] = '\0'; /* wipe out incorrect entry */
                  }
                  if (!sscanf(bcy[j],"%lf",&Y[j])) {
                    fprintf(stderr,"Invalid Northing : %s\n",bcy[j]);
                    ask_again = 1;
                    bcy[j][0] = '\0'; /* wipe out incorrect entry */
                  }
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
              if (pj_do_proj(X+j,Y+j,&info_ll,&info_coord) < 0) {
                sprintf(buff,"Could not convert %s %s to coord",bcx[j],bcy[j]);
                fprintf(stderr,buff);
                last_words(CM,-1);
              }
            }
            bx[j] = X[j];
            by[j] = Y[j];
          }
        }  /* if ll_ask */
        else
#endif
	  if ((n_points =  ask_map_coor (n_points)) < 0)
		return(-1) ;

#ifdef LATLON
  if (ll_flag == 0) {
    for (j=0; j<n_points; j++) {
      sprintf(bcx[j],"%lf",bx[j]);
      sprintf(bcy[j],"%lf",by[j]);
    }
  }
#endif
	Clear_info() ;

	/*  go to curses page  and register points  */
/*DEBUG*/ 
	debugf ("entering register_map_coords\n");
	status =  register_map_coor( n_points) ;
/*DEBUG*/ 	debugf ("status = %d\n", status);
	if (status < 0)
		return (-1) ;
	if (status == 1)
		break ;

 }		/*  while (1)   */


	Clear_base () ;
	Clear_info () ;

	if ( (fp = fopen (coor_file, "w"))  != NULL)
	{
		save_coor_to_file (fp) ;
		fclose (fp) ;
	}

	 flush_keyboard() ;
	 return(0) ;

}			/*  init_map ()  */



