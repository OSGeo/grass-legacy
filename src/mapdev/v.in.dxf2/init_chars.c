#include "dxf2vect.h"

int dxf_init_chars (void)
{
	strcpy (zzero, "0");
	strcpy (eeight, "8");
	strcpy (tten, "10");
	strcpy (ttwenty, "20");
	strcpy (eelev, "11");
	strcpy (ttwentyone, "21");
	strcpy (header, "HEADER");
	strcpy (extmax, "$EXTMAX");
	strcpy (extmin, "$EXTMIN");
	strcpy (entitie, "ENTITIES");
	strcpy (polyline, "POLYLINE");
	strcpy (line, "LINE");
        strcpy (arc, "ARC");
	strcpy (circle, "CIRCLE");
	strcpy (text, "TEXT");
	strcpy (point, "POINT");
	strcpy (vertex, "VERTEX");
	strcpy (seqend, "SEQEND");

	return 0;
}
