/* %W% %G% */
/* get_stp_proj.c    1.0   5/22/91
*    Created by : M.L.Holko , Soil Conservation Service, USDA
*    Purpose: function
*             Provides a means of getting desired projection
*             parameters for st.plane projections
*    Input arguements : empty string
*    Output arguements: projection parameters in string
*
*/
#include <stdio.h>
#include <ctype.h>

main (argc,argv)
int argc;
char *argv[];
{
int ST_code;

ST_code = get_stp_num(0,0);
fprintf (stdout,"%d\n",ST_code);
}

