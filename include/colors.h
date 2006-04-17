#ifndef GRASS_COLORS_H
#define GRASS_COLORS_H

#define FIXED		0
#define FLOAT		1

/* Don't add more colors here.
   These colors are the preallocated colors used in displays for efficiency.
   Adding colors here reduces the number of colors it is possible for a user to display.
   If support for named colors is needed it should go in G_str_to_color. */

#define BLACK		1
#define RED		2
#define GREEN		3
#define BLUE		4
#define YELLOW		5
#define CYAN		6
#define MAGENTA		7
#define WHITE		8
#define GRAY		9
#define ORANGE		10
#define AQUA		11
#define INDIGO		12
#define VIOLET		13
#define BROWN		14

#define GREY            GRAY
#define PURPLE          VIOLET

/* increase when adding more colors */
#define MAX_COLOR_NUM 14
#define MAXCOLORS MAX_COLOR_NUM
/* This is the number of color names. It is one higher due to grey/gray */
#define MAX_COLOR_NAMES 15

/* These can be in any order. They must match the lookup strings in the table below. */
#define D_COLOR_LIST "red,orange,yellow,green,blue,indigo,violet,white,black,gray,brown,magenta,aqua,grey,cyan,purple"

/* max length of color string */
#define MAX_COLOR_LEN 32

/* The order in this table is important! It will be indexed by color number */
static struct {
    unsigned char r, g, b;
} standard_colors_rgb [MAX_COLOR_NUM + 1] =
{
  {  0,  0,  0}, /* This is a dummy value to make lookup easier */
  {  0,  0,  0}, /* BLACK   */
  {255,  0,  0}, /* RED     */
  {  0,255,  0}, /* GREEN   */
  {  0,  0,255}, /* BLUE    */
  {255,255,  0}, /* YELLOW  */
  {  0,255,255}, /* CYAN    */
  {255,  0,255}, /* MAGENTA */
  {255,255,255}, /* WHITE   */
  {128,128,128}, /* GRAY    */
  {255,128,  0}, /* ORANGE  */
  {100,128,255}, /* AQUA    */
  {  0,128,255}, /* INDIGO  */
  {128,  0,255}, /* VIOLET  */
  {180, 77, 25}  /* BROWN   */
};

/* The order in this table has no meaning. */
static struct {
    char *name;
    int number;
} standard_color_names[MAX_COLOR_NAMES] =
{
    {"black",   BLACK},
    {"red",     RED},
    {"green",   GREEN},
    {"blue",    BLUE},
    {"yellow",  YELLOW},
    {"cyan",    CYAN},
    {"magenta", MAGENTA},
    {"white",   WHITE},
    {"grey",    GREY},
    {"gray",    GRAY},
    {"orange",  ORANGE},
    {"aqua",    AQUA},
    {"indigo",  INDIGO},
    {"violet",  VIOLET},
    {"purple",  PURPLE},
    {"brown",   BROWN}
};

#endif 

