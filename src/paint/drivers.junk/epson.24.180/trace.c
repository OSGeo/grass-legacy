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
	    case 'O': fprintf (stdout,"skip perforation\n"); break;
	    case 'J': fprintf (stdout,"move down %d\n", getchar()); break;
	    case '$':
		    c = getchar();
		    c += getchar() * 256;
		    fprintf (stdout,"set absolute print position %d\n",c);
		    break;
	    case '*':
		    fprintf (stdout,"select graphics mode %d\n", c = getchar());
		    raster = getchar();
		    raster += getchar() * 256;
		    fprintf (stdout,"  %d columns\n", raster);
		    if (c >= 32) raster *= 3;
		    break;
	    case 'r':
		    fprintf (stdout,"select color %d\n", getchar()); break;
	    
	    default: fprintf (stdout,"ESC %o\n", c); break;
	    }
	else
	    fprintf (stdout,"%c",c);
    }
}
