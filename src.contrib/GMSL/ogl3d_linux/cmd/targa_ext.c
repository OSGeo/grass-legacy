targa_extension (fp, Window)
    FILE *fp;
    struct Cell_head *Window;
{
    char buffer[500];
    int i;

    Ext_offset = ftell (fp);

    /* Field 10  Extension Size   2 bytes */
    fwrite_short (fp, 494);		/* 494 bytes for version 2.0 tga */

    /* Field 11  Image Author Name */
    sprintf (buffer, "%41s", G_whoami());
    fwrite (buffer, 1, 42, fp);		/* 41 chars plus NULL */

    /* Field 12  Author Comments  324 Bytes */
    /* Note later fields depend on buffer being full of zeros */
    for (i = 0 ; i < 324 ; i++)
	buffer[i] = 0;
/**/fwrite (buffer, 1, 324, fp);		/* 4 lines 80 chars + nuls */

    /* Field 13  Date/Time        12 bytes */
/**/fwrite (buffer, 1, 12, fp);		       /* 6 shorts  (not used here) */

    /* Field 14   Job Name/ID     41 bytes */
/**/fwrite (buffer, 1, 41, fp);		       /* (not used here) */

    /* Field 15   Job Time         6 bytes */
/**/fwrite (buffer, 1, 6, fp);		       /* (not used here) */
    
    /* Field 16   Software ID     41 Bytes */
    fwrite ("Cell2tga  USA-CERL  GRASS               ", 1, 41, fp);

    /* Field 17   Software Version  3 bytes */
    fwrite_short (fp, 100);			/* Version 1.00 */
    fwrite (" ", 1, 1, fp);

    /* Field 18   Key Color 	   4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* Black */

    /* Field 19  Pixel Aspect Ratio 4 bytes 
    fwrite_short (fp, (short) Window->ew_res);	
    fwrite_short (fp, (short) Window->ns_res);  */ 

    /* Field 20   Gamma Value     4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 21   Color Corr. Offset  4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 22   Postage Stamp Offset 4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 23   Scan Line Offset    4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 24   Attributes Type     1 byte */
/**/fwrite (buffer, 1, 1, fp);			/* not used */

}
