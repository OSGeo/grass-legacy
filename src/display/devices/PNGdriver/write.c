
#include <stdio.h>
#include <stdlib.h>
#include <png.h>

#include "gis.h"
#include "pngdriver.h"

static void
write_png(void)
{
	static jmp_buf jbuf;
	static png_struct *png_ptr;
	static png_info *info_ptr;
	FILE *output;
	int x, y;
	unsigned int *p;
	png_bytep line;
	const char *str;
	int compress;

	png_ptr = png_create_write_struct(
		PNG_LIBPNG_VER_STRING, &jbuf, NULL, NULL);
	if (!png_ptr)
		G_fatal_error("PNG: couldn't allocate PNG structure");

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr)
		G_fatal_error("PNG: couldn't allocate PNG structure");

	if (setjmp(png_jmpbuf(png_ptr)))
		G_fatal_error("error writing PNG file");

	output = fopen(file_name, "wb");
	if (!output)
		G_fatal_error("PNG: couldn't open output file %s", file_name);

	png_init_io(png_ptr, output);

	png_set_IHDR(
		png_ptr, info_ptr,
		width, height, 8,
		true_color ? PNG_COLOR_TYPE_RGB_ALPHA : PNG_COLOR_TYPE_PALETTE,
		PNG_INTERLACE_NONE,
		PNG_COMPRESSION_TYPE_DEFAULT,
		PNG_FILTER_TYPE_DEFAULT);

	if (!true_color)
	{
		png_color png_pal[256];
		png_byte trans = (png_byte) transparent;
		int i;

		for (i = 0; i < 256; i++)
		{
			png_pal[i].red   = palette[i][0];
			png_pal[i].green = palette[i][1];
			png_pal[i].blue  = palette[i][2];
		}

		png_set_PLTE(png_ptr, info_ptr, png_pal, 256);

		png_set_tRNS(png_ptr, info_ptr, &trans, 1, NULL);
	}

	png_set_invert_alpha(png_ptr);

	str = getenv("GRASS_PNG_COMPRESSION");
	if (str && sscanf(str, "%d", &compress) == 1)
		png_set_compression_level(png_ptr, compress);

	png_write_info(png_ptr, info_ptr);

	line = G_malloc(width * 4);

	for (y = 0, p = grid; y < height; y++)
	{
		png_bytep q = line;

		if (true_color)
			for (x = 0; x < width; x++, p++)
			{
				unsigned int c = *p;
				*q++ = (png_byte) (c >> 16);
				*q++ = (png_byte) (c >>  8);
				*q++ = (png_byte) (c >>  0);
				*q++ = (png_byte) (c >> 24);
			}
		else
			for (x = 0; x < width; x++, p++, q++)
				*q = (png_byte) *p;

		png_write_row(png_ptr, line);
	}

	G_free(line);

	png_write_end(png_ptr, info_ptr);

	png_destroy_write_struct(&png_ptr, &info_ptr);

	fclose(output);
}

static void
write_ppm(void)
{
	FILE *output;
	int x, y;
	unsigned int *p;

	output = fopen(file_name, "wb");
	if (!output)
		G_fatal_error("PNG: couldn't open output file %s", file_name);

	fprintf(output, "P6\n%d %d\n255\n", width, height);

	for (y = 0, p = grid; y < height; y++)
	{
		for (x = 0; x < width; x++, p++)
		{
			unsigned int c = *p;

			if (true_color)
			{
				fputc((unsigned char) (c >> 16), output);
				fputc((unsigned char) (c >>  8), output);
				fputc((unsigned char) (c >>  0), output);
			}
			else
			{
				fputc(palette[c][0], output);
				fputc(palette[c][1], output);
				fputc(palette[c][2], output);
			}
		}
	}

	fclose(output);
}

static void
write_pgm(void)
{
	char *mask_name = G_store(file_name);
	FILE *output;
	int x, y;
	unsigned int *p;

	mask_name[strlen(mask_name) - 2] = 'g';

	output = fopen(mask_name, "wb");
	if (!output)
		G_fatal_error("PNG: couldn't open mask file %s", mask_name);

	G_free(mask_name);

	fprintf(output, "P5\n%d %d\n255\n", width, height);

	for (y = 0, p = grid; y < height; y++)
	{
		for (x = 0; x < width; x++, p++)
		{
			unsigned int c = *p;

			if (true_color)
				fputc(255 - (unsigned char) (c >> 24), output);
			else
				fputc(255 - palette[c][3], output);
		}
	}

	fclose(output);
}

void
write_image(void)
{
	char *p = file_name + strlen(file_name) - 4;

	if (!modified)
		return;

	if (G_strcasecmp(p, ".png") == 0)
		write_png();
	else if (G_strcasecmp(p, ".ppm") == 0)
	{
		write_ppm();
		if (has_alpha)
			write_pgm();
	}
	else
		G_fatal_error("Graph_Close: unknown file type: %s", p);

	modified = 0;
}

