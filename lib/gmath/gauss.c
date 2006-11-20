#include <math.h>
#include <grass/gmath.h>


/*!
 * \fn float gauss (int seed)
 *
 * \brief Gaussian random number generator
 *
 * Gaussian random number generator (mean = 0.0, sigma = 1.0)
 *
 * \param seed
 * \return float
 */

float gauss (int seed)
{
	static int 	iset;
	static float	gset;
	float		fac,r,v1,v2;

	if (iset==0)
	{
		do
		{
			v1=2.0*rand1(seed)-1.0;
			v2=2.0*rand1(seed)-1.0;
			r=v1*v1+v2*v2;
		}
		while (r>=1.0);

		fac=sqrt(-2.0*log(r)/r);
		gset=v1*fac;
		iset=1;

		return(v2*fac);
	}

	iset=0;

	return gset;
}
