/* Header file: map_info.h
**
** Author: Paul W. Carlson	April 1992
*/

struct map_info {
	double x, y;
	char *font;
	int size;
	int color;
};

#ifdef MAIN
struct map_info m_info;
#else
extern struct map_info m_info;
#endif
