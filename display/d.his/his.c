#define LEVELS	10             /* Based on what 8 bits can accomodate   */
#define LEV1	LEVELS         /* LEVEL */
#define LEV2	LEV1 * LEV1    /* LEVEL*LEVEL */
#define LEV	LEVELS + 1  
#include "gis.h"

static unsigned char *pass_buff_r ;
static unsigned char *pass_buff_g ;
static unsigned char *pass_buff_b ;

/****************************************************************************
 * G_HIS() returns the lookup table value for the proper HIS color associated
 *   with the following values:
 *    HUE:
 *       R:      red percent. value 0 - 1.0 
 *       G:      grn percent. value 0 - 1.0 
 *       B:      blu percent. value 0 - 1.0 
 *    INTENSITY:
 *       I    intensity value:  0 (black) to 1.0 (full color)
 *    SATURATION:
 *       S     saturation val:   0 (gray) to 1.0 (full color)
 *
 * G_make_HIS_color() generates the associated color lookup table 
 ****************************************************************************/

int G_HIS (
    int col,
    int R,     /* red percent. for hue: value 0 - 1.00 */
    int G,     /* grn percent. for hue: value 0 - 1.00 */
    int B,     /* blu percent. for hue: value 0 - 1.00 */
    int I,  /* intensity value: 0 (black) to 255 (white)     */
    int S  /* saturation val:  0 (gray) to 255 (full color) */
)
{
	static int extra_r = 0  ;
	static int extra_g = 0  ;
	static int extra_b = 0  ;

	if(col==0)
		extra_r = extra_g = extra_b = 0 ;

/*
	if (I == 127)
		;
	else if (I > 127)
	{
		I = (I - 128) * 2 ;
		R = R + (255 - R) * I / 255 ;
		G = G + (255 - G) * I / 255 ;
		B = B + (255 - B) * I / 255 ;
	}
	else
	{
		R = R * I / 127 ;
		G = G * I / 127 ;
		B = B * I / 127 ;
	}
*/
	R = (int)(R * I / 255.) ;
	G = (int)(G * I / 255.) ;
	B = (int)(B * I / 255.) ;

	/* modify according to saturation */
	if (S != 255)
	{
		S = 255 - S ;
		R = R - (R - 127) * S / 255 ;
		G = G - (G - 127) * S / 255 ;
		B = B - (B - 127) * S / 255 ;
	}

	/* make sure final values are within range */
	if (R < 0) R = 0 ;
	if (G < 0) G = 0 ;
	if (B < 0) B = 0 ;
	if (R >= 255) R = 255 ;
	if (G >= 255) G = 255 ;
	if (B >= 255) B = 255 ;

	/* Calculate color lookup table value */
	{
		register int tmp ;
		int red, grn, blu ;

		tmp = R + pass_buff_r[col] + extra_r ;
		if (tmp > 255)
		{
			extra_r = tmp-255 ;
			tmp=255 ;
		}
		red = LEVELS*tmp/256 ;
		extra_r = pass_buff_r[col] = (unsigned char)(extra_r + tmp - red * 256 / LEVELS) / 2 ;

		tmp = G + pass_buff_g[col] + extra_g ;
		if (tmp > 255)
		{
			extra_g = tmp-255 ;
			tmp=255 ;
		}
		grn = LEVELS*tmp/256 ;
		extra_g = pass_buff_g[col] = (unsigned char)(extra_g + tmp - grn * 256 / LEVELS) / 2 ;

		tmp = B + pass_buff_b[col] + extra_b ;
		if (tmp > 255)
		{
			extra_b = tmp-255 ;
			tmp=255 ;
		}
		blu = LEVELS*tmp/256 ;
		extra_b = pass_buff_b[col] = (unsigned char)(extra_b + tmp - blu * 256 / LEVELS) / 2 ;

		return (red*LEV2 + grn*LEV1 + blu) ;
	}
}

int G_make_HIS_color (struct Colors *color)
{
	int i ;
	int r, g, b ;
	float incr ;

	i = 0;
	incr = 255 / (LEVELS - 1) ;

	G_init_colors(color) ;

	for (r=0; r<LEVELS; r++)
		for (g=0; g<LEVELS; g++)
			for (b=0; b<LEVELS; b++)
				G_set_color((CELL)i++,(int)(r*incr), (int)(g*incr), (int)(b*incr), color) ;

	return 0;
}

int 
alloc_pass_buff (int n)
{
	register unsigned char *ptr ;
	register int i ;
	char *malloc() ;

	pass_buff_r = (unsigned char *)malloc(n) ;
	pass_buff_g = (unsigned char *)malloc(n) ;
	pass_buff_b = (unsigned char *)malloc(n) ;
	if (pass_buff_r == NULL || pass_buff_g == NULL || pass_buff_b == NULL)
		G_fatal_error("Insufficient memory for pass_buff") ;

	ptr = pass_buff_r ;
	i = n ;
	while(i--)
		*ptr++ = 0 ;

	ptr = pass_buff_g ;
	i = n ;
	while(i--)
		*ptr++ = 0 ;

	ptr = pass_buff_b ;
	i = n ;
	while(i--)
		*ptr++ = 0 ;

	return 0;
}
