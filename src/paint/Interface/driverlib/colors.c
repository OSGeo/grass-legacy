static int nlevels = 5;
static int ncolors = 125;

int Pset_color_levels (int n)
{
    nlevels = n;
    ncolors = n*n*n;

    return 0;
}

int Pcolorlevels (int *red, int *grn, int *blu)
{
    *red = nlevels;
    *grn = nlevels;
    *blu = nlevels;

    return 0;
}

int Pcolormultipliers (int *red, int *grn, int *blu)
{
    *red = nlevels*nlevels;
    *grn = nlevels;
    *blu = 1;

    return 0;
}

int Pcolornum (double red, double green, double blue)
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

int Pcolorvalue (int n, float *red, float *grn, float *blu)
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

    return 0;
}

int Pncolors (void)
{
    return ncolors;
}
