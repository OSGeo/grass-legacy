struct window *max_dist(ptr)   
	
	struct window *ptr;
{
	extern char buf[];
	extern int txt_a,txt_b; 
	extern double m_dist;   

	sprintf(buf,"Enter max distance for los analysis (metres) : "); 

	throw_text(buf,txt_a,txt_b);
	input();
	sscanf(buf,"%lf",&m_dist); 
	/* printf("\n max_dist = %lf\n",m_dist);   */
	return(ptr);
	}





