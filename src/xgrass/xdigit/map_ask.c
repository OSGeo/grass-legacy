#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include    "map.h"
#include    "digit.h"
#include    <stdio.h>

ask_map_coor ()
{
	int i, k ;
	int coor_cnt ;
	int at_line ;
	int at_point[MAX_COOR] ;
	int  status ;
	char tmp[50] ;

/*DEBUG*/ debugf ("ask_map_coor\n");
	/*  number of coordinates we can handle.  this may be the second time
	*   to this menu and some points may have been registered
	*/

    coor_cnt = MAX_COOR - reg_cnt ;

    /*  show min needed and max they can go to  */
    sprintf(tmp,"    Enter %d - %d points :  points registered %d",
		(MIN_COOR > reg_cnt) ? MIN_COOR - reg_cnt : 0  , 
		coor_cnt, reg_cnt);

    if (show_coor_ask(tmp) < 0)
        return (-1);
    return (shrink_map_coor());
}



ask_map_coor_ll ()
{
	int i, k ;
	int coor_cnt ;
	int at_line ;
	int at_point[MAX_COOR] ;
	int  status ;
	char tmp[50] ;

/*DEBUG*/ debugf ("ask_map_coor\n");
	/*  number of coordinates we can handle.  this may be the second time
	*   to this menu and some points may have been registered
	*/

    coor_cnt = MAX_COOR - reg_cnt ;

    /*  show min needed and max they can go to  */
    sprintf(tmp,"    Enter %d - %d points :  points registered %d",
		(MIN_COOR > reg_cnt) ? MIN_COOR - reg_cnt : 0  , 
		coor_cnt, reg_cnt);

    while((k=show_coor_ask(tmp)) == 2);
    if (k < 0)
        return (-1);
    return (shrink_map_coor_ll());
}








shrink_map_coor_ll()
{

	int  i, k ;
            
        
	for ( i = 0, k = 0; i < MAX_COOR; i++)
	{
                G_strip(bcx[i]);
                G_strip(bcy[i]);
		if ((strcmp(bcx[i],"0.0")==0 && strncmp(bcy[i],"0.0")==0)
		   || (strcmp(bcx[i],"0.")==0 && strncmp(bcy[i],"0.")==0)
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



shrink_map_coor()
{

	int  i, k ;
	get_coors();
/* first put coord from bcx,bcy to bx,by or b_lat,b_lon */
	for ( i = 0, k = 0; i < MAX_COOR; i++)
	{
		if ( bx[i] == 0.0  ||  by[i] == 0.0)
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
