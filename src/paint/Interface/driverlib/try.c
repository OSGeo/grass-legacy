main()
{
    int n,r,g,b;
    float fr,fg,fb;

    for (n=0; n < 125; n++)
    {
	    Pcolorvalue (n, &fr, &fg, &fb);
	    r = fr * 5;
	    g = fg * 5;
	    b = fb * 5;
	    printf ("%d = (%d,%d,%d) = (%f,%f,%f)\n",
		    n,r,g,b,fr,fg,fb);
    }
}
