main()
{
    float r,g,b;
    unsigned char red, grn, blu;
    int n;
    int ncolors;

    ncolors = Pncolors();
    for (n = 0; n < ncolors; n++)
    {
	Pcolorvalue (n, &r, &g, &b);
	red = (int) (r*255);
	grn = (int) (g*255);
	blu = (int) (b*255);

	fprintf (stdout,"%d: %f(%d) %f(%d) %f(%d)\n", n, r, red, g, grn, b, blu);
    }
}
