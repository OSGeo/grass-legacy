#include "gis.h"
#include "windround.h"

/* function to modify the resolution of the database window	*/
struct window *resolution(ptr)
	struct window *ptr;
{
	extern struct Cell_head window;
	extern txt_a,txt_b;
	extern char buf[];

sprintf(buf,"Current RESOLUTION: %10.2lf.  Enter new value or <cr> for no change :",window.ns_res);
	throw_text(buf,txt_a,txt_b);
	input();
	if(buf[0] == '\0') return(ptr);
	else  sscanf(buf,"%lf",&window.ns_res);
	window.ew_res = window.ns_res;
	return(ptr);
}


/* function to modify the northing of the database window	*/
struct window *northing(ptr)

	struct window *ptr;
{
	extern struct Cell_head window;
	extern txt_a,txt_b;
	extern char buf[];
	
sprintf(buf,"Current NORTHING: %10.2lf.  Enter new value or <cr> for no change : ",window.north);
	throw_text(buf,txt_a,txt_b);
        input();
	if(buf[0] == '\0') return(ptr);
	else  sscanf(buf,"%lf",&window.north);
	return(ptr);
}



/* function to modify the southing of the database window       */
struct window *southing(ptr)

        struct window *ptr;
{
        extern struct Cell_head window;
        extern txt_a,txt_b;
        extern char buf[];
        
sprintf(buf,"Current SOUTHING: %10.2lf.  Enter new value or <cr> for no change : ",window.south);
        throw_text(buf,txt_a,txt_b);
        input();
        if(buf[0] == '\0') return(ptr);
        else  sscanf(buf,"%lf",&window.south);
        return(ptr);
}


/* function to modify the easting of the database window       */
struct window *easting(ptr) 
 
        struct window *ptr;
{ 
        extern struct Cell_head window; 
        extern txt_a,txt_b; 
        extern char buf[]; 
         
sprintf(buf,"Current EASTING: %10.2lf.  Enter new value or <cr> for no change : ",window.east);
        throw_text(buf,txt_a,txt_b); 
        input(); 
        if(buf[0] == '\0') return(ptr); 
        else  sscanf(buf,"%lf",&window.east); 
        return(ptr); 
} 


/* function to modify the westing of the database window       */
struct window *westing(ptr) 
 
        struct window *ptr;
{ 
        extern struct Cell_head window; 
        extern txt_a,txt_b; 
        extern char buf[]; 
         
sprintf(buf,"Current WESTING: %10.2lf.  Enter new value or <cr> for no change : ",window.west);
        throw_text(buf,txt_a,txt_b); 
        input(); 
        if(buf[0] == '\0') return(ptr); 
        else  sscanf(buf,"%lf",&window.west); 
        return(ptr); 
} 



struct window *adj_window(ptr)
	struct window *ptr;
{
	extern struct Cell_head window;
	extern txt_a,txt_b;
	extern char buf[];


    if ( window.ns_res <= 0  || window.ew_res <= 0 )
	{
        sprintf(buf,"ERROR: Illegal resolution value(s) specifed") ;
	throw_text(buf,txt_a,txt_b);
	sleep(4);
	erase_in_poly(ptr);
	G_get_set_window(&window);
	return(ptr);
	}

    if (window.north <= window.south)
	{
        sprintf(buf,"ERROR: North must be larger than south");
	throw_text(buf,txt_a,txt_b);
	sleep(4);
	erase_in_poly(ptr);
	G_get_set_window(&window);
	return(ptr);
	}

    if (window.east <= window.west)
	{
        sprintf(buf,"ERROR: East must be larger than west");
	throw_text(buf,txt_a,txt_b);
	sleep(4);
	erase_in_poly(ptr);
	G_get_set_window(&window);
	return(ptr);
	}

/* if the north-south is not multiple of the resolution,
 *    round the south downward
 */
    window.rows = (window.north - window.south) / window.ns_res
                   + WINDOW_ROUND ;
    window.south = window.north - window.rows * window.ns_res;

/* do the same for the west */
    window.cols = (window.east - window.west) / window.ew_res
                   + WINDOW_ROUND ;
    window.west = window.east - window.cols * window.ew_res;
	G_set_window(&window);
	return(ptr);

}

