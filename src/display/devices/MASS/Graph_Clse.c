Graph_Close()
{
/* turn cursor on */
	/*
	fprintf (stdout,"Gc") ;
	*/

/* deassign graphics processor */
	Set_plane_10_to_white() ;
	mgideagp(0,0) ;
}
