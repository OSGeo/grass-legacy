/****************************************************************/
/*								*/
/*	outline_box.c	in	~/src/i_range			*/
/*								*/
/*	This function draws the border of a box whose 		*/
/*	dimensions are specified by screen coordinates 		*/
/*	that define the top, bottom, left and right edges	*/
/*	of the box.						*/
/*								*/
/****************************************************************/


outline_box(t,b,l,r)
	int t,b,l,r;
{
        char *malloc();
        int *x_ptr, *y_ptr;

	/*  assign space for arrays to contain x and y coors.	*/
        x_ptr = (int *) malloc(5*sizeof(int));
        y_ptr = (int *) malloc(5*sizeof(int));


	/*	prepare arrays with correct coordinates		*/
        *x_ptr = l;
        *y_ptr = t;
        *(x_ptr + 1) = r;
        *(y_ptr + 1) = t;
        *(x_ptr + 2) = r;
        *(y_ptr + 2) = b;
        *(x_ptr + 3) = l;
        *(y_ptr + 3) = b;
        *(x_ptr + 4) = *(x_ptr);
        *(y_ptr + 4) = *(y_ptr);

	/*	draw border					*/
        R_polyline_abs(x_ptr,y_ptr,5);
	R_flush();
}

/************ END OF FUNCTION "OUTLINE_BOX" *********************/

