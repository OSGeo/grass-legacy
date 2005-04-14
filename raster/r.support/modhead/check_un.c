#include "config.h"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "glocale.h"
#include "local_proto.h"


/*
 * check_uncompressed(struct Cell_head *, long)
 * checks uncompressed file and offers valid combinations
 *
 * RETURN: 0 success : 1 failure
 */
int check_uncompressed(struct Cell_head *cellhd, long filesize)
{
    long filesize_calc;
    FILE *fd;
    static char *tempfile = NULL;
    char command[256];

    /* Check for bad file size */
    filesize_calc = cellhd->rows * cellhd->cols * cellhd->format;
    if (filesize_calc == filesize)
        return EXIT_FAILURE;

    /* Create temporary file */
    if (tempfile == NULL)
        tempfile = G_tempfile();

    /* Write valid combinations to temp file */
    fd = fopen(tempfile, "w");
    fprintf(fd, _("The product of the rows(%d), cols(%d) and bytes per "
                "cell(%d) = %ld\n"),
                cellhd->rows, cellhd->cols, cellhd->format, filesize_calc);
    fprintf(fd, _("does not equal the file size (%ld)\n"), filesize);
    fprintf(fd, _("The following combinations will produce the "
                "correct file size:\n\n"));

    if (cellhd->format == 0 || (filesize % cellhd->format != 0)) {
        int i;

        for (i = 1; i <= sizeof(CELL); i++) {
            if (filesize % i)
                continue;

            fprintf(fd, _("%d byte%s per cell\n"), i, i == 1 ? _("") : _("s"));
            factors(fd, filesize, i);
        }
    } else {
        fprintf(fd, _("%d byte%s per cell\n"), cellhd->format,
                      cellhd->format == 1 ? _("") : _("s"));
        factors(fd, filesize, cellhd->format);
    }

    /* Close temp file */
    fclose(fd);

    /* display valid combinations */
    snprintf(command, sizeof(command), "$GRASS_PAGER %s", tempfile);
    G_system(command);

    /* remove temp file */
    unlink(tempfile);

    return EXIT_SUCCESS;
}
