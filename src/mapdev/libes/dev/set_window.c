/*  @(#)set_window.c	2.1  6/26/87  */
set_window()
{
	char buff[256] ;
	double Ux1, Uy1 ;
	double Ux2, Uy2 ;
	double N, S, E, W ;

	Clear_info() ;
	Write_info(1, "Identify corners of graphics window.") ;
	Write_info(2, "  Locate digitizer cursor on one corner of map.") ;
	Write_info(3, "  Then hit <RETURN>") ;
	Get_curses_text(buff) ;

	coll_a_pnt ( &Ux1, &Uy1) ;

	Clear_info() ;
	Write_info(2, "  Now place digitizer cursor on diagonal corner of map.") ;
	Write_info(3, "  Then hit <RETURN>") ;
	Get_curses_text(buff) ;

	coll_a_pnt ( &Ux2, &Uy2) ;

	Clear_info() ;

	N = Uy2 > Uy1 ? Uy2 : Uy1 ;
	S = Uy2 < Uy1 ? Uy2 : Uy1 ;
	E = Ux2 > Ux1 ? Ux2 : Ux1 ;
	W = Ux2 < Ux1 ? Ux2 : Ux1 ;

	window_conversions(N, S, E, W) ;
}
