/* %W% %G% */
#include <stdio.h>

method_text()
{
char line[100];

G_clear_screen ();
fprintf(stderr,
"Now that you have specified the number of areas you are interested in, it is\n");
fprintf(stderr,
"time to define each area, each being added to the data layer separately.\n\n");
fprintf(stderr,
"You will have several options as to how to define an area of interest:\n\n");
fprintf(stderr,
"1) You may choose to define a circle as an area of interest, specifying\n");
fprintf(stderr,
"   the center (coordinates) and radius of the circle.\n\n");
fprintf(stderr,
"2) You may choose to define a regular polygon as an area of interest,\n");
fprintf(stderr,
"   specifying the center (coordinates) and radius of a circle which\n");
fprintf(stderr,
"   circumscribes the polygon and the number of sides the polygon will have.\n\n");
fprintf(stderr,
"3) You may choose to define a specific polygonal area, specifying the\n");
fprintf(stderr,
"   number of edges the area is to have (up to 50) and the coordinates \n");
fprintf(stderr,
"   for each corner of the area.\n\n");
fprintf(stderr,
"When you are ready to define your first area of interest, hit <return>\n");
gets(line);
}
