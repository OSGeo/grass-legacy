struct window *obs_elev(ptr) 
	
	struct window *ptr;
{
	extern char buf[];
	extern int txt_a,txt_b; 
	extern double ob_elev;   

	sprintf(buf,"Enter the elevation above ground (meters) : "); 

	throw_text(buf,txt_a,txt_b);
	input();
	sscanf(buf,"%lf",&ob_elev);
	/* printf("\n obs_elev= %lf\n",ob_elev); */
	return(ptr);
	}


struct window *velocity(ptr)

	struct window *ptr;
{
	extern char buf[];
	extern int txt_a, txt_b;
	extern double vel_initial;

	sprintf(buf,"Enter the muzzle velocity (meters/sec) : ");

	throw_text(buf,txt_a,txt_b);
	input();
	sscanf(buf,"%lf",&vel_initial);
	/* printf("\n vel_initial = %lf", vel_initial); */
	return(ptr);
}







