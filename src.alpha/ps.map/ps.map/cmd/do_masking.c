/* Function: do_masking
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_info.h"
extern char *ps_mask_file;

do_masking()
{
    FILE *ps_mask_fp;
    int rows, cols, r, g, b;
    double factor, width;
    char buf[128];

    /* open the temporary mask file */
    if ((ps_mask_fp = fopen(ps_mask_file, "r")) == NULL)
    {
        printf("\nCan't open temporary PostScript mask file.\n");
        exit(-1);
    }

    /* adjust columns to multiple of 8 */
    rows = G_window_rows();
    cols = G_window_cols();
    while (cols % 8) cols++;
    factor = (double)cols / (double)G_window_cols();
    width = factor * PS.map_pix_wide;

    /* write mask to PostScript file, using "no data" color */
    fprintf(PS.fp, "gsave\n");
    fprintf(PS.fp, "/imgstrg %d string def\n", cols / 8);
    fprintf(PS.fp, "/cw %d def /ch %d def\n", cols, rows);
    fprintf(PS.fp, "%.2lf %.2lf TR\n", PS.map_left, PS.map_bot);
    fprintf(PS.fp, "%d %d scale\n", 
	(int)(width + 0.5), (int)(PS.map_pix_high + 0.5));
    fprintf(PS.fp, "%.3lf %.3lf %.3lf C\n", PS.r0, PS.g0, PS.b0);
    fprintf(PS.fp, "cw ch true\n");
    fprintf(PS.fp, "[cw 0 0 ch neg 0 ch]\n");
    fprintf(PS.fp, "{currentfile imgstrg readhexstring pop}\n");
    fprintf(PS.fp, "imagemask\n");
    while (fgets(buf, 128, ps_mask_fp) != NULL) fprintf(PS.fp, "%s", buf);
    fprintf(PS.fp, "grestore\n");

    /* close and remove temporary mask file */
    fclose(ps_mask_fp);
    unlink(ps_mask_file);
}
