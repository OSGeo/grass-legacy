begin_paint(prows, pcols)
{
    Praster();
    Ppictsize (prows, pcols);
    build_color_tables();
    build_dither_tables();
}

paint(red, grn, blu, row, pcols)
    unsigned char *red, *grn, *blu;
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
}
