#include <stdio.h>
#include "viewstruct.h"


layerScale(layer,currentScale)
double currentScale;
LAYER *layer;
{

	if ( (currentScale >= layer->scaleDownThreshold) && (currentScale <= layer->scaleUpThreshold) )
		return(True);
	return(False);
}
