
typedef struct {
    int    field;	
    int    color, bgcolor, bcolor;
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
