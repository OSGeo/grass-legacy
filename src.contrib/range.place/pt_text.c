#include "gis.h"


struct window *viewpt_text(ptr)
	
	struct window *ptr;
{
	extern int iv,jv;
	extern double viewpt_x,viewpt_y;
	extern struct Cell_head window;
	extern char buf[];
	extern txt_a,txt_b;
        sprintf(buf,"Enter the UTM coordinates (x and y) : ");
	throw_text(buf,txt_a,txt_b);

	read_in_values(&viewpt_x,&viewpt_y);

	iv = (window.north - viewpt_y)/window.ns_res;
        jv = (viewpt_x - window.west)/window.ew_res;

	return(ptr);
	}



struct window *firingpt_text(ptr)
	struct window *ptr;
{
        extern int ifir,jfir;
        extern double firingpt_x,firingpt_y; 
        extern struct Cell_head window;
	 extern char buf[]; 
        extern txt_a,txt_b; 
        sprintf(buf,"Enter the UTM coordinates (x and y) : "); 
        throw_text(buf,txt_a,txt_b); 

	read_in_values(&firingpt_x,&firingpt_y);

	ifir = (window.north - firingpt_y)/window.ns_res;
        jfir = (firingpt_x - window.west)/window.ew_res;

	return(ptr);
}


struct window *azimuth(ptr)
	struct window *ptr;
{
	extern double azimuth1, azimuth2;
	 extern char buf[];  
        extern txt_a,txt_b;  
        sprintf(buf,"Enter the azimuth angles (degrees) : "); 
        throw_text(buf,txt_a,txt_b);  

	read_in_values(&azimuth1,&azimuth2);
	return(ptr);
}

struct window *altitude(ptr) 
        struct window *ptr;
{
        extern double high_angle, low_angle; 
         extern char buf[];  
        extern txt_a,txt_b;   
        sprintf(buf,"Enter the low and high angles of fire(degrees) : ");  
        throw_text(buf,txt_a,txt_b);   

        read_in_values(&low_angle,&high_angle); 
        return(ptr); 
} 






read_in_values(ptr_x,ptr_y)
	double *ptr_x, *ptr_y;

{
	char str1[40];
	extern char buf[];

        input();
        sscanf(buf,"%[^ ] %lf",str1,ptr_y);
        sscanf(str1,"%lf",ptr_x);
        /* printf("\n pt_x=%lf, pt_y=%lf\n",*ptr_x,*ptr_y); */

}

 
