#include "driver.h"

float transparency = 0.0;

float COM_Transparency(float trans)
{
	float ret = transparency;

	if (!support_transparency)
		return ret;

	if (trans >= 0.0 && trans <= 1.0)
		transparency = trans;

	return ret;
}

