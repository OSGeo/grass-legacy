/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	int amc_conversion(curve_number,amc)

	To convert the AMC CN II to the AMC CN amc (either 1 or 3)
	and returns the new CN.
*/

#include "CN.h"


int amc_conversion(curve_number,amc)
int	curve_number, amc;
{
	int	new_cn,temp_var;
	double	exp();
	double  temp_var1;

	temp_var = 100 - curve_number;

	if(amc == 1)
	{
	    temp_var1 = (double) exp(2.533 - 0.063 * temp_var);

	    new_cn = (int) (curve_number - ((20 * temp_var)/ (temp_var + temp_var1)));
	}
	else
	{
	    temp_var1 = (double) exp(0.00673 * temp_var);

	    new_cn = (int)(curve_number * temp_var1);
	}

	return(new_cn);
}

