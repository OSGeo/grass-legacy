#include <stdio.h>

#include "config.h"
#include "driver.h"
#include "driverlib.h"

#define FILE_NAME  "map.png"

extern char *file_name;
extern FILE *output;
extern int currentColor;
extern unsigned int *xpixels;
extern int true_color;
extern int auto_write;
extern int has_alpha;

extern int width, height;
extern unsigned int *grid;
extern unsigned char palette[256][4];
extern unsigned int transparent;
extern int modified;

extern void write_image(void);
void InitColorTableFixed(void);

