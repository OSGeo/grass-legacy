#define LEVELS	10             /* Based on what 8 bits can accomodate   */
#define LEV1	LEVELS         /* LEVEL */
#define LEV2	LEV1 * LEV1    /* LEVEL*LEVEL */
#define LEV	LEVELS + 1  
#include "gis.h"

static unsigned char *pass_buff_r ;
static unsigned char *pass_buff_g ;
static unsigned char *pass_buff_b ;

G_RGB(col, R, G, B)
	int col ;
	int R ;     /* red percent. for hue: value 0 - 1.00 */
	int G ;     /* grn percent. for hue: value 0 - 1.00 */
	int B ;     /* blu percent. for hue: value 0 - 1.00 */
{
	static unsigned char extra_r = 0  ;
	static unsigned char extra_g = 0  ;
	static unsigned char extra_b = 0  ;
	register int tmp ;
	int red, grn, blu ;

	if(col=0)
		extra_r = extra_g = extra_b = 0 ;

	tmp = R + pass_buff_r[col] + extra_r ;
		if (tmp > 255)
		{
			extra_r = tmp-255 ;
			tmp=255 ;
		}
		red = LEVELS*tmp/256 ;
		extra_r = pass_buff_r[col] = (extra_r + tmp - red * 256 / LEVELS) / 2 ;

	tmp = G + pass_buff_g[col] + extra_g ;
		if (tmp > 255)
		{
			extra_g = tmp-255 ;
			tmp=255 ;
		}
		grn = LEVELS*tmp/256 ;
		extra_g = pass_buff_g[col] = (extra_g + tmp - grn * 256 / LEVELS) / 2 ;

	tmp = B + pass_buff_b[col] + extra_b ;
		if (tmp > 255)
		{
			extra_b = tmp-255 ;
			tmp=255 ;
		}
		blu = LEVELS*tmp/256 ;
		extra_b = pass_buff_b[col] = (extra_b + tmp - blu * 256 / LEVELS) / 2 ;

	return (red*LEV2 + grn*LEV1 + blu) ;
}

G_make_RGB_color(color)
	struct Colors *color ;
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
}

alloc_pass_buff(n)
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
}
