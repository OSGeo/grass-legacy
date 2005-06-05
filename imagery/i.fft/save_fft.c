#include "gis.h"
#include "glocale.h"
#include "globals.h"


int save_fft (int total, double *data[2], double *maximum, double *minimum)
{
        int i;
        double max, min, *temp;
        char buffer[100];
        FILE *fp;

        max = *maximum;
        min = *minimum;

        sprintf(buffer, "cell_misc/%s", Cellmap_real);
        if ((fp = G_fopen_new(buffer, FFTREAL)) == NULL)
                G_fatal_error(_("Unable to open file in the cell_misc directory."));
        fwrite((char *) data[0], sizeof(double), (size_t)total, fp);
        fclose(fp);

        sprintf(buffer, "cell_misc/%s", Cellmap_imag);
        if ((fp = G_fopen_new(buffer, FFTIMAG)) == NULL)
                G_fatal_error(_("Unable to open file in the cell_misc directory."));
        fwrite((char *) data[1], sizeof(double), (size_t)total, fp);
        fclose(fp);

        temp = data[0] ;
        for (i=0; i<total; i++, temp++) {
                max = (max > *temp) ? max : *temp;
                min = (min < *temp) ? min : *temp;
        }

        temp = data[1] ;
        for (i=0; i<total; i++, temp++) {
                max = (max > *temp) ? max : *temp;
                min = (min < *temp) ? min : *temp;
        }

        *maximum = max;
        *minimum = min;

	return 0;
}
