/* %W% %G% */
/*  Rewritten by Philip Thompson */

#define MAIN
#include "usgs.h"
#undef  MAIN

main(argc, argv)
char *argv[];
{
    int new, nrow;

    G_gisinit("DEM Tape Extraction Program");
    /* check command line for accuracy */
    if (!getargs(argc, argv)) {
        fprintf(stderr,"usage: %s [if=tapedev] of=cellfile\n [-nowin]",
                argv[0]);
        exit(1);
    }
    /* initialize variables and buffers */
    if (!usgs_init())
        exit(1);

    /*  if any data found, call getgrid to extract it   */
    fprintf(stderr, "Reading Elevation data...");
#ifdef DEBUG
    fprintf(stderr, "\n");
    (void)fflush(stdout);
    (void)fflush(stderr);
#endif
    if (!getgrid()) {
        fprintf(stderr,"main: could not find data in correct file");
        exit(1);
    }
    (void)fclose(tapefile);
    (void)close(fd);
    fprintf(stderr, "Done\n");

    /* make system call to Mrot90 to rotate cell file 90 deg */
    (void)sprintf(command, "Mrot90 if=%s of=%s bpc=%d rows=%d cols=%d",
        inf, of, sizeof(CELL), cellhd.cols, cellhd.rows);
    fprintf(stderr, "%s\n", command);
    if (system(command)) {
        (void)unlink(inf);
        G_fatal_error("can't rotate cell file");
    }
    (void)unlink(inf);

    /* open new cell file */
    fprintf(stderr, "copying rotated file to cell file\n");
    if ((new = G_open_cell_new(outname)) < 0)
        G_fatal_error("can't create new cell file ");
    /* copy rotated file into rcell file */
    fd = open(of, 0);
    for (nrow = 0; nrow < cellhd.rows; nrow++) {
        (void)read(fd, (char*)profile_buf, cellhd.cols * sizeof(CELL));
        if (G_put_map_row(new, profile_buf, nrow) < 0)
            G_fatal_error("error while writing to cell file");
    }
    (void)close(fd);
    (void)unlink(of);
    fprintf(stderr, "CREATING SUPPORT FILES FOR %s\n", outname);
    G_close_cell(new);
    exit(0);
}
