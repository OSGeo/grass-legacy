#define EXTERN extern

#include "gis.h"
#include "edit.h"

show_weapons()
{

    int i;
    char buf[100];

    G_clear_screen();
    printf("      CHOSEN WEAPON TYPES FOR THIS RUN OF BNOISE\n\n");
    printf("  Code:             Weapon Name:\n\n");

    for (i=0; i<num_weapons; i++)
    {
        printf("   %d                %s\n",weapons[i].code,weapons[i].name);

        if (((i+1)/9) * 9 == (i+1) && (i < (num_weapons-1)))
        {

            printf(" Hit return to continue...");
            G_gets(buf);

            G_clear_screen();
            printf("      CHOSEN WEAPON TYPES FOR THIS RUN OF BNOISE\n\n");
            printf("  Code:             Weapon Name:\n\n");

        }

    }

    printf(" Hit return to continue...");
    G_gets(buf);

}
