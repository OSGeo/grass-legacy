/****************************************************************/
/* disp_matrix()	Function to display the contents of	*/
/*			the three matrices used in solving 	*/
/*			the set of linear equations.		*/
/* 			V.1.0, Jo Wood, 9th December, 1994.   	*/
/****************************************************************/

#include "param.h"

disp_matrix(a,x,z,n)
    float **a,x[],z[];
    int	   n;
			/* Displays matrices used to solve a 
			   set of linear equations in the form 

			 _                        _      _  _      _  _ 
			| a(1,1) a(1,2) ... a(1,n) |    | x1 |    | z1 |
			| a(2,1) a(2,2) ... a(2,n) |    | x2 |    | z2 |
			|    :	    :   ...   :    | .  | :  | =  | :  |
			|    :	    :   ...   :    |    | :  |    | :  |
			| a(n,1) a(n,2) ... a(n,n) |    | xn |    | zn |
			 -                        -      -  -      -  -

								*/
{

    int row,col;		/* Counts through the matrix */ 
    char dummy[128];		/* Kewboard input (pause) */

    for (row=1; row<=n; row++)
    {
	printf("[ ");

	for (col=1; col<=n; col++)
	    printf("%.3f\t",a[row][col]/1);	

	printf("]\t[ %.0f\t]\t[ %.0f\t]\n",x[row],z[row]);
    }
    printf("\n\n");

    gets(dummy);
}

disp_wind(z)
    CELL *z;			/* Local window */
{

    int row,col;		/* Count through local window. */
    char dummy[128];


    for (row=0; row<wsize; row++)
    {
	for (col=0; col<wsize; col++)
	    printf("%d\t",*(z + (row*wsize) + col));

	printf("\n");
    }

    gets(dummy);
}
