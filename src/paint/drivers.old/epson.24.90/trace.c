#define ESC 033
main()
{
    int c;
    int raster = 0;

    while ((c = getchar()) >= 0)
    {
	if (raster)
	{
	    raster--;
	    continue;
	}
	if (c == ESC)
	    switch (c = getchar())
	    {
	    case 'O': printf ("skip perforation\n"); break;
	    case 'J': printf ("move down %d\n", getchar()); break;
	    case '$':
		    c = getchar();
		    c += getchar() * 256;
		    printf ("set absolute print position %d\n",c);
		    break;
	    case '*':
		    printf ("select graphics mode %d\n", c = getchar());
		    raster = getchar();
		    raster += getchar() * 256;
		    printf ("  %d columns\n", raster);
		    if (c >= 32) raster *= 3;
		    break;
	    case 'r':
		    printf ("select color %d\n", getchar()); break;
	    
	    default: printf ("ESC %o\n", c); break;
	    }
	else
	    printf ("%c",c);
    }
}
