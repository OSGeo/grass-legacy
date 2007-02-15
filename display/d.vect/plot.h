/* TODO: should use 24bit instead of 16 colors, maybe implement
   predefined color tables? */
struct rgb_color {
        unsigned char R, G, B;
       };
static const int palette_ncolors = 16;

static struct rgb_color palette[16] =  {
	{198, 198, 198}, /*  1: light gray */
	{127, 127, 127}, /*  2: medium/dark gray */
	{255,   0,   0}, /*  3: bright red */
	{139,   0,   0}, /*  4: dark red */
	{  0, 255,   0}, /*  5: bright green */
	{  0, 139,   0}, /*  6: dark green */
	{  0,   0, 255}, /*  7: bright blue */
	{  0,   0, 139}, /*  8: dark blue   */
	{255, 255,   0}, /*  9: yellow */
	{139, 126,  10}, /* 10: olivey brown */
	{255, 165,   0}, /* 11: orange */
	{255, 192, 203}, /* 12: pink   */
	{255,   0, 255}, /* 13: magenta */
	{139,   0, 139}, /* 14: dark magenta */
	{  0, 255, 255}, /* 15: cyan */
	{  0, 139, 139}  /* 16: dark cyan */
};

typedef struct {
    int    field;	
    int    has_bgcolor;
    int    has_bcolor;
    struct rgb_color color, bgcolor, bcolor;
    int    size;
    char   *font;
    int    xref, yref;
} LATTR;

#define LCENTER  0
#define LLEFT    1
#define LRIGHT   2
#define LBOTTOM  3
#define LTOP     4


#define DISP_SHAPE 0x01
#define DISP_CAT   0x02
#define DISP_TOPO  0x04
#define DISP_DIR   0x08
#define DISP_ATTR  0x10
#define DISP_ZCOOR 0x20

#define RENDER_GPP	0
#define RENDER_RPA	1
#define RENDER_DP	2
#define RENDER_DPC	3

extern int render;
