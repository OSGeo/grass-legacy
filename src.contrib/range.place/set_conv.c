

struct window *set_conv(ptr)

	struct window *ptr;

{

	extern struct Cell_head window;
	extern double dlg_U_south,dlg_U_north,dlg_U_west,
			dlg_U_east,dlg_D_west,dlg_D_east,
			dlg_D_north,dlg_D_south,dlg_U_to_D_xconv,
			 dlg_U_to_D_yconv;
	int t,b,l,r;
	

	double D_get_u_to_d_xconv() ;   /* variables    */
        double D_get_u_to_d_yconv() ;   /* to           */
        double D_get_u_west() ;         /* hold         */
        double D_get_u_east() ;         /* information  */
        double D_get_u_north() ;        /* about        */
        double D_get_u_south() ;        /* the          */
        double D_get_d_west() ;         /* conversion   */
        double D_get_d_east() ;         /* of           */
        double D_get_d_north() ;        /* coordinates  */
        double D_get_d_south() ;



	D_get_screen_window(&t,&b,&l,&r);
	D_do_conversions(&window,t,b,l,r);
	
	dlg_D_west  = D_get_d_west() ;
        dlg_D_east  = D_get_d_east() ;
        dlg_D_north = D_get_d_north() ;
        dlg_D_south = D_get_d_south() ;
        dlg_U_west  = D_get_u_west() ;
        dlg_U_east  = D_get_u_east() ;
        dlg_U_north = D_get_u_north() ;
        dlg_U_south = D_get_u_south() ;
        dlg_U_to_D_xconv = D_get_u_to_d_xconv() ;
        dlg_U_to_D_yconv = D_get_u_to_d_yconv() ;

	return(ptr);
}
