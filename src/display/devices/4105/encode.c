#define HIY	xy[0]
#define EXT	xy[1]
#define LOY	xy[2]
#define HIX	xy[3]
#define LOX	xy[4]
/*
4095. (tek x resolution) /542. (6300 x res for square image) = 7.55535055
3132. (tek y resolution) /399. (6300 x res for square image) = 7.84962406
 638 references every pixle
 542 gives square image
*/

#define XCONV	(2 * 7.55535055)
#define YCONV	(2 * 7.84962406)

extern int SCREEN_BOTTOM ;

char *
encode_xy(x, y)
	register int x, y ;
{
	static char xy[10] ;

	x = (int) ((float)x * XCONV) ;
	y = (int) ((float)(SCREEN_BOTTOM - y) * YCONV) ;
	if (x<0) x = 0 ;
	if (x>4095) x = 4095 ;
	if (y<0) y = 0 ;
	if (y>4095) y = 4095 ;
	EXT = (char)(((x & 03) | ((y & 03) << 2)) | 0140);
	HIX = (char)(((x >> 7) & 037) | 040);
	LOX = (char)(((x >> 2) & 037) | 0100);
	HIY = (char)(((y >> 7) & 037) | 040);
	LOY = (char)(((y >> 2) & 037) | 0140);
	xy[5] = (char)00 ;
	return(xy) ;
}

char *
encode(x)
{
	char xy[10] ;
	register int t ;

	t = (x >> 10) & 037 ;
	xy[0] = (char)(t | 0100);
	t = (x >> 4) & 077 ;
	xy[1] = (char)(t | 0100);
	t = x & 017 ;
	if (x < 0)
		xy[2] = (char)(t | 040);
	else
		xy[2] = (char)(t | 060);
	xy[3] = (char)00 ;
	return(xy) ;
}

tek_to_screen(x, y, wx, wy)
	int x, y, *wx, *wy ;
{
	/*
	*wx = (int)((float)(x*4) / XCONV) ;
	*wy = (int)((float)(y*4) / YCONV) ;
	*/
	*wx = (int)((float)(x*4) / XCONV) ;
	*wy = SCREEN_BOTTOM - (int)((float)(y*4) / YCONV) ;
}
