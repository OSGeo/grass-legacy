
#include "psdriver.h"

void PS_RGB_raster(
	int num, int nrows,
	const unsigned char *red, const unsigned char *grn, const unsigned char *blu,
	const unsigned char *nul)
{
	const char *type = true_color
		? (nul ? "RASTERRGBMASK" : "RASTERRGB")
		: (nul ? "RASTERGRAYMASK" : "RASTERGRAY");
	int i;

	output("gsave\n");
	output("%d %d moveto 1 %d scale\n", cur_x, cur_y, nrows);

	output("%d 1 [%d 0 0 1 0 0] %s\n", num, num, type);

	for (i = 0; i < num; i++)
	{
		if (true_color)
		{
			if (nul)
				output("%02X%02X%02X%02X", nul[i] ? 0xFF : 0x00, red[i], grn[i], blu[i]);
			else
				output("%02X%02X%02X", red[i], grn[i], blu[i]);
		}
		else
		{
			unsigned int gray = (unsigned int) (red[i] * 0.299 + grn[i] * 0.587 + blu[i] * 0.114);

			if (nul)
				output("%02X%02X", nul[i] ? 0xFF : 0x00, gray);
			else
				output("%02X", gray);
		}
	}

	output("\n");
	output("grestore\n");
}

