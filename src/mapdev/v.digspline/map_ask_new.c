#include    <stdio.h>
#include    "map.h"
#include "digit.h"


ask_map_coor_ll (n_points)
	int  n_points ;
{
	int i, k ;
	int coor_cnt ;
	int at_line ;
	int at_point[MAX_COOR] ;
	int  status ;
	char tmp[50] ;
        char	*star = "*" ;

	/*
	 *  close down curses so that Vask will work
	 */

	vask_suspend() ;

	/*  number of coordinates we can handle.  this may be the second time
	*   to this menu and some points may have been registered
	*/

	coor_cnt = MAX_COOR - reg_cnt ;

	/*  show min needed and max they can go to  */
	sprintf(tmp,"    Enter %d - %d points :  points registered %d",
		(MIN_COOR > reg_cnt) ? MIN_COOR - reg_cnt : 0  , coor_cnt, reg_cnt);

	V_clear();
	V_line(1,"          MAP REGISTRATION POINTS ");
	V_line(2, tmp);
        if (ll_ask == 2) {  /* reg file exists and is in lat/lon */
	  V_line(3, "Coordinates should be in Latitude/Longtitude" );
  	  V_line(5," Expected format for coordinates is dd:mm:ss.ffh");
  	  V_line(6," where h is 'n' or 's' for Lat and 'e' or 'w' for Lon ");
  	  V_line(7," dd, mm, ss have to be >= 0; use h to define hemisphere");

	  V_line(9,"    Point #        Latitude               Longtitude");
        }
        else {     /* Then ll_ask is 1 and user should be given a choice */
	  V_line(3, " Enter 1 if points are in Lat/Lon and 0 otherwise:" );
	  V_ques ( &ll_flag, 'i', 3, 50, 2);
  	  V_line(5,"Latitude/Longtitude points shouild be in dd:mm:ss.ffh format");
  	  V_line(6,"where h is 'n' or 's' for Lat and 'e' or 'w' for Lon ");
  	  V_line(7,"dd, mm, ss have to be >= 0; use h to define hemisphere");
	  V_line(9,"    Point #     X coord (or Lat)         Y coord (or Lon)");
        }


	for ( i=0; i < MAX_COOR; i++)
	{
		at_line = i + 10 ;
		at_point[i] = i + 1 ;

		if (use[i])
		{
			V_const (star, 's', at_line, 6, 6);
			V_const ( bcx[i], 's', at_line, 15, 20);
			V_const ( bcy[i], 's', at_line, 40, 20);

		}
		else
		{
			V_const (&at_point[i], 'i', at_line, 6, 6);
			V_ques ( bcx[i], 's', at_line, 15, 20);
			V_ques ( bcy[i], 's', at_line, 40, 20);
		}
	}

	V_line(at_line + 2, "      Those marked by '*' are registered.");

	V_intrpt_ok(); 

	/* add message before exit */
	if (!V_call()) 
	{
		fprintf(stderr,"ask_map_coor:  Leaving session.. \n");
		sleep(2);
		return(-1);
	}


	/*
	 * back to CURSES environment
	 */
	vask_respend() ;

	return ( shrink_map_coor_ll()) ;
}


shrink_map_coor_ll()
{

	int  i, k ;
            
        
	for ( i = 0, k = 0; i < MAX_COOR; i++)
	{
                G_strip(bcx[i]);
                G_strip(bcy[i]);
		if ((strcmp(bcx[i],"0.0")==0 && strncmp(bcy[i],"0.0",3)==0)
		   || (strcmp(bcx[i],"0.")==0 && strncmp(bcy[i],"0.",2)==0)
		   || (bcx[i][0]=='\0' && bcy[i][0]=='\0'))
			continue ;

		/*  same place count it, but skip it  */
		if( i == k)
		{
			++k ;
			continue ;
		}
		
		/*  valid point store them  */
		sprintf(bcx[k],"%s",bcx[i]) ;
		sprintf(bcy[k],"%s",bcy[i]) ;
		*(ax+k) = *(ax+i) ;
		*(ay+k) = *(ay+i) ;
		*(use+k) = *(use+i) ;
		*(residuals+k) = *(residuals+i) ;
		++k ;
	}

/*  now make sure everything else is zero'ed out  */
	i =  (k <= 0)  ?  0  :  k ;
	for ( ; i < MAX_COOR ; i++)
	{
	     if(!ll_flag) {	
		sprintf(*(bcx+i),"0.0");
		sprintf(*(bcy+i),"0.0");
             }
	     else {	
		bcx[i][0] = '\0';
		bcy[i][0] = '\0';
             }
		*(ax+i) =  0.0 ;
		*(ay+i) =  0.0 ;
		*(use+i) =  0 ;
		*(residuals+i) =  0.0 ;
	}

	return(k) ;

}
