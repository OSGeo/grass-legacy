/****************************************************************

NAME:		Pcolornum()

FUNCTION:	convert rgb % into a color number

USAGE:		Pcolornum (red, grn, blu)

		float red, grn, blu;


RETURNS:	integer in range 0-124
*****************************************************************/

Pcolornum (red, grn, blu)
    float red, grn, blu ;
{
    register int r;
    register int g;
    register int b;

    r = red * 5.;
    g = grn * 5.;
    b = blu * 5.;

    if (r < 0) r = 0;
    if (r > 4) r = 4;

    if (g < 0) g = 0;
    if (g > 4) g = 4;

    if (b < 0) b = 0;
    if (b > 4) b = 4;

    return (r*25 + g*5 + b);
}
