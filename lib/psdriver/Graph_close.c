/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */

#include "psdriver.h"

void PS_Graph_close(void)
{
	output("END\n");
	fclose(outfp);
}

