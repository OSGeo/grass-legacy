/* %W% %G% */
#include "P.h"
Pinit()
{
	int i, j, k;
	int two = 2;
	int four = 4;
	float red, green, blue;

	i = Pncolors();
	k = 1;

	for (j = 0; j < i ; j++, k++){
		Pcolorvalue (j, &red, &green, &blue);
		colrgn_ (&two, &red, &green, &blue, &k, &four);
	}
	
}
