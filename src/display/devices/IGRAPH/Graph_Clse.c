/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */

extern int WNO ;

Graph_Close()
{
	showcursor(WNO) ;
	Exit_tools() ;
}
