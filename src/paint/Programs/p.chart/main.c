main (argc, argv) char *argv[];
{
    int BLOCKSIZE;
    int BLOCKSPACE;
    int NBLOCKS;
    int TEXTSPACE;
    int FUDGE;
    int PRINTERWIDTH;

    unsigned char white;
    unsigned char black;
    int ncolors ;

    char buf[200];
    char temp[40];

    int pixels;

    int b;
    int block;
    int cols;
    int rr,cc;
    int left;
    int len;
    int lines;
    int nblocks;
    int r;
    int rows;



    G_gisinit(argv[0]);
    printf ("set printer and hit RETURN -->");
    if (!gets(buf)) exit(0);

    Pconnect();
    Plock();
    Popen();

    ncolors = Pncolors();
    if (ncolors > 256) ncolors = 256;
    if (ncolors <= 0) return;

    white = Pcolornum (1.0, 1.0, 1.0) ;
    black = Pcolornum (0.0, 0.0, 0.0) ;

    Pnpixels(&rows, &pixels) ;
    if (rows)
    {
	printf ("This function NOT available for current PAINT device\n");
	exit(1);
    }

    PRINTERWIDTH = Pnchars();
    BLOCKSIZE    = Pblocksize();
    BLOCKSPACE   = Pblockspace();
    NBLOCKS      = Pnblocks();
    TEXTSPACE    = Ptextspace();
    FUDGE        = Ptextfudge();


    rows  = (ncolors + NBLOCKS - 1) / NBLOCKS ;
    lines = rows * BLOCKSIZE + 1;

    Praster ();
    Ppictsize (lines, pixels);

    block = 0;
    for (r = 0;  r < rows; r++)
    {
        nblocks = ncolors - block;
        if (nblocks > NBLOCKS) nblocks = NBLOCKS;
        Praster ();

/* top line of block outlines */

        Prle_begin();

        left = pixels;
        for (b = 0; b < nblocks; b++)
        {
            Prle(black, BLOCKSIZE);
            Prle(white, BLOCKSPACE);
            left -= BLOCKSIZE + BLOCKSPACE;
	    if (FUDGE > 0 && b%FUDGE)
	    {
		Prle(white, 1);
		left--;
	    }
        }

        Prle (white, left);
        Prle_end () ;

/* body of the blocks with left, right borders */

        for (rr=0; rr < BLOCKSIZE-2; rr++)
        {
            Prle_begin() ;

            left = pixels;
            for (b = 0; b < nblocks; b++)
            {
                unsigned char num;

                num = block+b;

                Prle(black, 1);
		for (cc = 0; cc < BLOCKSIZE-2; cc++)
		    Prle(num,1);
                Prle(black, 1);
                Prle(white, BLOCKSPACE);
                left -= BLOCKSIZE + BLOCKSPACE;
		if (FUDGE > 0 && b%FUDGE)
		{
		    Prle(white, 1);
		    left--;
		}
            }

            Prle (white, left);
            Prle_end () ;
        }

/* bottom line of block outlines */

        Prle_begin();

        left = pixels;
        for (b = 0; b < nblocks; b++)
        {
            Prle(black, BLOCKSIZE);
            Prle(white, BLOCKSPACE);
            left -= BLOCKSIZE + BLOCKSPACE;
	    if (FUDGE > 0 && b%FUDGE)
	    {
		Prle(white, 1);
		left--;
	    }
        }

        Prle (white, left);
        Prle_end () ;

/* numbers below the blocks */

        Palpha();
        *buf = 0;
        for (b = 0; b < nblocks; b++)
        {
            sprintf(temp, "%3d%*s", block++, TEXTSPACE, "");
            strcat (buf, temp);
        }
        Ptext (buf);
    }

    Praster();
    Prle_begin ();
    Prle (white, pixels);
    Prle_end ();

    Pclose();
    Pdisconnect();
}
