#define EXTERN extern

#include "gis.h"
#include "edit.h"

show_targets()
{

    int i;
    char buf[100];

    G_clear_screen();
    printf("      CHOSEN TARGET POINTS FOR THIS RUN OF BNOISE\n\n");
    printf("   Id:         Easting:        Northing:\n\n");

    for (i=0; i<num_targets; i++)
    {
        printf("   %s      %lf       %lf\n",targets[i].id,targets[i].easting,
          targets[i].northing);

        if (((i+1)/9) * 9 == (i+1) && (i < (num_targets-1)))
        {

            printf("\n Hit return to continue...");
            G_gets(buf);

            G_clear_screen();
            printf("      CHOSEN TARGET POINTS FOR THIS RUN OF BNOISE\n\n");
            printf("   Id:         Easting:        Northing:\n\n");

        }

    }

    printf("\n Hit return to continue...");
    G_gets(buf);

}
