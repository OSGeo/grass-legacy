
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <grass/gis.h>
#include "pngdriver.h"

void read_ppm(void)
{
	FILE *input;
	int x, y;
	int i_width, i_height, maxval;
	unsigned int *p;

	if (!true_color)
		G_fatal_error("PNG: cannot use PPM/PGM with indexed color");

	input = fopen(file_name, "rb");
	if (!input)
		G_fatal_error("PNG: couldn't open input file %s", file_name);

	if (fscanf(input, "P6 %d %d %d", &i_width, &i_height, &maxval) != 3)
		G_fatal_error("PNG: invalid input file %s", file_name);

	fgetc(input);

	if (i_width != width || i_height != height)
		G_fatal_error(
			"PNG: input file has incorrect dimensions: expected: %dx%d got: %dx%d",
			width, height, i_width, i_height);

	for (y = 0, p = grid; y < height; y++)
	{
		for (x = 0; x < width; x++, p++)
		{
			unsigned int c = *p;

			int r = fgetc(input);
			int g = fgetc(input);
			int b = fgetc(input);

			r = r * 255 / maxval;
			g = g * 255 / maxval;
			b = b * 255 / maxval;

			c &= 0xFF000000;
			c |= (r << 16) | (g << 8) | (b << 0);

			*p = c;
		}
	}

	fclose(input);
}

void read_pgm(void)
{
	char *mask_name = G_store(file_name);
	FILE *input;
	int x, y;
	int i_width, i_height, maxval;
	unsigned int *p;

	if (!true_color)
		G_fatal_error("PNG: cannot use PPM/PGM with indexed color");

	mask_name[strlen(mask_name) - 2] = 'g';

	input = fopen(mask_name, "rb");
	if (!input)
		G_fatal_error("PNG: couldn't open input mask file %s", mask_name);

	if (fscanf(input, "P5 %d %d %d", &i_width, &i_height, &maxval) != 3)
		G_fatal_error("PNG: invalid input mask file %s", mask_name);

	fgetc(input);

	if (i_width != width || i_height != height)
		G_fatal_error(
			"PNG: input mask file has incorrect dimensions: expected: %dx%d got: %dx%d",
			width, height, i_width, i_height);

	G_free(mask_name);

	for (y = 0, p = grid; y < height; y++)
	{
		for (x = 0; x < width; x++, p++)
		{
			unsigned int c = *p;

			int k = fgetc(input);

			k = k * 255 / maxval;

			c &= 0x00FFFFFF;
			c |= (k << 24);

			*p = c;
		}
	}

	fclose(input);
}

