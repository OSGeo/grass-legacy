static int nlevels = 5;
static int ncolors = 125;

Pset_color_levels(n)
{
    nlevels = n;
    ncolors = n*n*n;
}

Pcolorlevels (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = nlevels;
    *grn = nlevels;
    *blu = nlevels;
}

Pcolormultipliers (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = nlevels*nlevels;
    *grn = nlevels;
    *blu = 1;
}

Pcolornum (red, green, blue)
    float red, green, blue ;
{
    register int r;
    register int g;
    register int b;
    int max;

    max = nlevels - 1;

    r = red * nlevels;
    g = green * nlevels;
    b = blue * nlevels;

    if (r < 0) r = 0;
    if (r > max) r = max;

    if (g < 0) g = 0;
    if (g > max) g = max;

    if (b < 0) b = 0;
    if (b > max) b = max;

    return (r*nlevels*nlevels + g*nlevels+ b);
}

Pcolorvalue (n, red, grn, blu)
    float *red, *grn, *blu;
{
    int r,g,b;
    int max, n2;
    double n1;

    n2 = nlevels * nlevels;
    max = ncolors - 1;
    n1 = nlevels - 1;

    if (n < 0) n = 0;
    if (n > max) n = max;

    r = n / n2 ;
    g = (n % n2) / nlevels;
    b = n % nlevels ;

    *red = r / n1 ;
    *grn = g / n1 ;
    *blu = b / n1 ;
}

Pncolors()
{
    return ncolors;
}
