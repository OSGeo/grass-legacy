#include "Paintlib.h"
#include "local_proto.h"

int begin_paint (int prows, int pcols)
{
    Praster();
    Ppictsize (prows, pcols);
    build_color_tables();
    build_dither_tables();

    return 0;
}

int paint (unsigned char *red, unsigned char *grn, unsigned char *blu,
    int row, int pcols)
{
    int c;
    red_dither (red, row, 0, pcols);
    grn_dither (grn, row, 0, pcols);
    blu_dither (blu, row, 0, pcols);

    Prle_begin();
    while (pcols-- > 0)
    {
	c = printer_color_number ((int)*red++, (int)*grn++, (int)*blu++);
	Prle ((unsigned char)c, 1);
    }
    Prle_end();

    return 0;
}
