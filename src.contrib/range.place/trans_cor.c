
/************************************************************************/ 
/*	trans_coors.c in ~/src/Gpat_place				*/
/************************************************************************/

/*	This function transforms the coordinates (x,y) according	*/
/*	to user defined rotation 'theta' about the origin of the 	*/
/*	pattern and 2-D translation defined by 'translation_x' and 'translation_y'.	*/


trans_coors( ptr_to_x, ptr_to_y )

	double *ptr_to_x, *ptr_to_y;

{
	double x,y;
	extern double translation_x, translation_y, 
		cos_theta, sin_theta, origin_x, origin_y;

	x= *ptr_to_x - origin_x;
	y= *ptr_to_y - origin_y;

	/*	TRANSFORMATION		*/
	*ptr_to_x = x * cos_theta - y * sin_theta + translation_x + origin_x;
	*ptr_to_y = x * sin_theta + y * cos_theta + translation_y + origin_y; 
} 
