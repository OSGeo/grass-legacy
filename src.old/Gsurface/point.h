/* %W% %G% */


/*	Data structure for a grid-cell whose value is known 	*/

		struct point
		{
		short row, col;
		float value;
		};



/*	Data structure for a cell neighboring the one whose 	*/
/*	value is being interpolated				*/

		struct neighbor
		{
		double distance;
		float value;
		struct neighbor *next;
		};

/****************************************************************/
