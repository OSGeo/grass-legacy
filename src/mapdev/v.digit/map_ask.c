#include <unistd.h>
#include    <stdio.h>
#include    "vask.h"
#include    "map.h"
#include    "dig_curses.h"
#include "local_proto.h"
#include "glocale.h"

char	*star = "*" ;

int 
ask_map_coor (int n_points)
{
	int i;
	int coor_cnt ;
	int at_line ;
	int at_point[MAX_COOR] ;
	char tmp[50] ;

	/*
	 *  close down curses so that Vask will work
	 */

	vask_suspend() ;

	/*  number of coordinates we can handle.  this may be the second time
	*   to this menu and some points may have been registered
	*/

	coor_cnt = MAX_COOR - reg_cnt ;

	/*  show min needed and max they can go to  */
	sprintf(tmp,_("    Enter %d - %d points :  points registered %d"),
		(MIN_COOR > reg_cnt) ? MIN_COOR - reg_cnt : 0  , coor_cnt, reg_cnt);
	V_clear();
	V_line(1,_("    MAP REGISTRATION POINTS "));
	V_line(2, tmp);
	V_line(4,_("    Point #       X coord       Y coord"));

	for ( i=0; i < MAX_COOR; i++)
	{
		at_line = i + 5 ;
		at_point[i] = i + 1 ;

		if (use[i])
		{
			V_const (star, 's', at_line, 6, 6);
			V_const ( &bx[i], 'd', at_line, 15, 12);
			V_const ( &by[i], 'd', at_line, 30, 12);

		}
		else
		{
			V_const (&at_point[i], 'i', at_line, 6, 6);
			V_ques ( &bx[i], 'd', at_line, 15, 12);
			V_ques ( &by[i], 'd', at_line, 30, 12);
		}
	}

	V_line(at_line + 2, _("      Enter 0.0 to delete a coordinate pair."));
	V_line(at_line + 3, _("      Those marked by '*' are registered."));

	V_intrpt_ok(); 

	/* add message before exit */
	if (!V_call()) 
	{
/*
		Old_tty ();
*/
		fprintf(stderr,"ask_map_coor:  Leaving session.. \n");
		sleep(2);
		return(-1);
	}


	/*
	 * back to CURSES environment
	 */
	vask_respend() ;

	return ( shrink_map_coor()) ;
}


int 
shrink_map_coor (void)
{

	int  i, k ;

	for ( i = 0, k = 0; i < MAX_COOR; i++)
	{
		if ( bx[i] == 0.0  &&  by[i] == 0.0)
			continue ;

		/*  same place count it, but skip it  */
		if( i == k)
		{
			++k ;
			continue ;
		}
		
		/*  valid point store them  */
		*(bx+k) = *(bx+i) ;
		*(by+k) = *(by+i) ;
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
		
		*(bx+i) =  0.0 ;
		*(by+i) =  0.0 ;
		*(ax+i) =  0.0 ;
		*(ay+i) =  0.0 ;
		*(use+i) =  0 ;
		*(residuals+i) =  0.0 ;
	}

	return(k) ;

}
