/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */

#include "psdriver.h"

void PS_Graph_close(void)
{
	output("%%%%BeginTrailer\n");
	output("END\n");
	output("%%%%EndTrailer\n");
	fclose(outfp);
}

