#include "gis.h"
#include "menu.h"

struct box *viewpt_mouse(ptr)

	struct box *ptr;
{
	extern int iv,jv;
	
	get_pt_from_screen(&iv,&jv);
	
	return(ptr);

}


struct box *firingpt_mouse(ptr)
	struct box *ptr;

{
	extern int ifir,jfir;

	get_pt_from_screen(&ifir,&jfir);

	return(ptr);
}



get_pt_from_screen(i,j) 
        int *i,*j;
{ 
        extern char buf[]; 
        extern txt_a,txt_b; 
        int map_top,map_bot,map_rite,map_left; 
        double D_get_d_north(),D_get_d_south(); 
        double D_get_d_east(),D_get_d_west(); 
        double D_d_to_a_row(),D_d_to_a_col(); 
        int t,b,l,r; 
        extern struct Cell_head window; 
        extern int screen_x,screen_y,button;
 
sprintf(buf,"Please click on the base map to specify location");
        throw_text(buf,txt_a,txt_b); 
	D_get_screen_window(&t,&b,&l,&r);
         /* printf("\n st=%d, sb=%d, sl=%d, sr=%d\n",t,b,l,r);  */
        D_do_conversions(&window,t,b,l,r); 
        map_top = D_get_d_north(); 
        map_bot = D_get_d_south(); 
        map_rite = D_get_d_east(); 
        map_left = D_get_d_west(); 
/* printf("\n t=%d, b=%d, l=%d, r=%d\n",map_top,map_bot,map_left,map_rite); */
 
        while(screen_x<map_left || screen_x>map_rite || 
                screen_y<map_top || screen_y>map_bot) 
        { 
        R_get_location_with_pointer(&screen_x,&screen_y,&button); 
        } 
 
        *i = D_d_to_a_row((double) screen_y); 
        *j = D_d_to_a_col((double) screen_x); 
         
        /* printf("\n i=%d        j=%d \n",*i,*j);  */
         
 
}


