/***************************************************************/
/*                                                             */
/*      point.h    in   ~/src/Gdrain                           */  
/*                                                             */
/*      This header file defines the point data structure      */  
/*      that contains the various attributes of a grid cell.   */
/*                                                             */
/***************************************************************/

	struct point
	{
    	int row, column;
	double slope;
    	struct point  *next;
	};

/***************************************************************/
