/* Function: make_procs
**
** This function creates some commonly used PostScript procedures.
**
** Author: Paul W. Carlson	March 1992
*/

#include "ps_info.h"

make_procs()
{
    int level;

    /* level 2 is default PostScript level */
    level = (PS.level != 1) ? 2 : 1;
    fprintf(PS.fp, "/level %d def\n", level);

    /* make definiton operators */
    fprintf(PS.fp, "/BD {bind def} bind def\n");
    fprintf(PS.fp, "/XD {exch def} BD\n");

    /* make increment and decrement operators */
    fprintf(PS.fp, "/IN {1 add} BD\n");
    fprintf(PS.fp, "/DE {1 sub} BD\n");

    /* exch sub operator */
    fprintf(PS.fp, "/XS {exch sub} BD\n");

    /* newpath moveto operator */
    fprintf(PS.fp, "/NM {newpath moveto} BD\n");

    /* closepath fill operator */
    fprintf(PS.fp, "/CF {closepath fill} BD\n");

    /* lineto operator */
    fprintf(PS.fp, "/LN {lineto} BD\n");

    /* divide by 2 */
    fprintf(PS.fp, "/D2 {2 div} BD\n");

    /* draw a line segment */
    /* assumes x2 y2 x1 y1 on top of stack */
    fprintf(PS.fp, "/L {moveto LN stroke} BD\n");

    /* adds a relative vector to the path*/
    /* assumes x2 y2 on top of stack */
    fprintf(PS.fp, "/V {rlineto} BD\n");

    /* construct rectangular box path */
    /* assumes l b r t on top of stack */
    fprintf(PS.fp, 
	"/B {4 1 roll 2 copy 5 index 5 index 8 1 roll\n");
    fprintf(PS.fp, 
	"NM LN LN LN closepath} BD\n");

    /* set line width */
    /* assumes line width is on top of stack */
    fprintf(PS.fp, "/W {setlinewidth} BD\n");

    /* stringwidth operator */
    fprintf(PS.fp, "/SW {stringwidth} BD\n");

    /* translate operator */
    fprintf(PS.fp, "/TR {translate} BD\n");

    /* set current color */
    /* assumes R G B values (0.0 to 1.0) on top of stack */
    fprintf(PS.fp, "/C {setrgbcolor} BD\n");

    /* set color to black, line width to 1 */
    fprintf(PS.fp, "/BW {0 0 0 C 1 W} BD\n");

    /* moveto and show */
    /* assumes text x y on top of stack */
    fprintf(PS.fp, "/MS {moveto show} BD\n");

    /* fill interior of current path with current color */
    fprintf(PS.fp, "/F {gsave fill grestore} BD\n");

    /* draw current path */
    fprintf(PS.fp, "/D {stroke} BD\n");

    /* set font name */
    /* assumes font name is on top of stack */
    fprintf(PS.fp, "/FN {/fn exch cvn def} BD\n");

    /* set font */
    /* assumes size is on top of stack */
    fprintf(PS.fp, "/SF {fn findfont exch scalefont setfont} BD\n");

    /* initialize font to Helvetica, size = 10 */
    fprintf(PS.fp, "(Helvetica) FN 10 SF\n");

    /* initialize the text box path */
    fprintf(PS.fp, "/ZB {0 0 NM gsave}  BD\n");

    /* get the path of the text box */
    fprintf(PS.fp, "/PB {dup false charpath flattenpath pathbbox\n");
    fprintf(PS.fp, "/bt exch mg add def ");
    fprintf(PS.fp, "/br exch mg add def\n");
    fprintf(PS.fp, "/bb exch mg sub def ");
    fprintf(PS.fp, "/bl exch mg sub def} BD\n"); 

    /* set the text box path */
    fprintf(PS.fp, "/TB {bl bb br bt B} BD\n");

    /* text in box */
    fprintf(PS.fp, "/TIB {0 0 MS grestore} BD\n");

    /* procs for text box path */
    fprintf(PS.fp, "/LTX {mg add} BD\n");
    fprintf(PS.fp, "/RTX {mg add br bl sub sub} BD\n");
    fprintf(PS.fp, "/CTX {mg add br bl sub D2 sub} BD\n");
    fprintf(PS.fp, "/UTY {mg add bt bb sub sub} BD\n");
    fprintf(PS.fp, "/LTY {mg add} BD\n");
    fprintf(PS.fp, "/CTY {mg add bt bb sub D2 sub} BD\n");

    /* proc used for highlight color */
    fprintf(PS.fp, "/HC {0 0 NM dup false charpath stroke} BD\n");

    /* all procs should be defined above this line */
    fprintf(PS.fp, "%%%%EndProlog\n");

}
