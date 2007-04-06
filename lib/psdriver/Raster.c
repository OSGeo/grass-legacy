
#include <string.h>

#include "psdriver.h"

static int src[2][2];
static int dst[2][2];

void PS_begin_scaled_raster(int s[2][2], int d[2][2])
{
	int ox, oy, sx, sy;

	memcpy(src, s, sizeof(src));
	memcpy(dst, d, sizeof(dst));

	sx = dst[0][1] - dst[0][0];
	sy = dst[1][1] - dst[1][0];
	ox = dst[0][0];
	oy = dst[1][0];

	output("gsave\n");
	output("%d %d translate %d %d scale\n", ox, oy, sx, sy);
}

int PS_scaled_raster(
	int n, int row,
	const unsigned char *red, const unsigned char *grn, const unsigned char *blu, const unsigned char *nul)
{
	const char *type = true_color
		? (nul ? "RASTERRGBMASK" : "RASTERRGB")
		: (nul ? "RASTERGRAYMASK" : "RASTERGRAY");
	int sx = src[0][1] - src[0][0];
	int sy = src[1][1] - src[1][0];
	int ox = src[0][0];
	int oy = src[1][0] - row;
	int i;

	output("%d 1 [%d 0 0 %d %d %d] %s\n", n, sx, sy, ox, oy, type);

	for (i = 0; i < n; i++)
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

	if (row + 1 >= src[1][1])
		output("grestore\n");

	return row + 1;
}

