#define EXTERN extern

#include "gis.h"
#include "edit.h"

show_fp()
{

    int i;
    char buf[100];

    G_clear_screen();
    printf("      CHOSEN FIRING POINTS FOR THIS RUN OF BNOISE\n\n");
    printf("                                             Number of Associated\n");
    printf("   Id:         Easting:             Northing:       Weapons\n\n");

    for (i=0; i<num_fp; i++)
    {
        printf("   %s      %lf       %lf        %d\n",
          firing[i].id,firing[i].easting,
          firing[i].northing, firing[i].num_weap);

        if (((i+1)/9) * 9 == (i+1) && (i < (num_fp-1)))
        {

            printf("\n Hit return to continue...");
            G_gets(buf);

            G_clear_screen();
            printf("      CHOSEN FIRING POINTS FOR THIS RUN OF BNOISE\n\n");
            printf("                                             Number of Associated\n");
            printf("   Id:         Easting:             Northing:       Weapons\n\n");

        }

    }

/*
{
int j;
for (i=0; i<num_fp; i++)
{
fprintf(stderr,"info for firing point number %d\n",i);
fprintf(stderr,"%s %lf %lf %f %d\n",
          firing[i].id, firing[i].easting,
          firing[i].northing, firing[i].gc_fact,
          firing[i].num_weap);
for (j=0; j<firing[i].num_weap; j++)
fprintf(stderr,"%d %d %d %d %d %s %s %f\n",
              firing[i].ptr[j].weap_code, firing[i].ptr[j].num_day,
              firing[i].ptr[j].num_night, firing[i].ptr[j].min_charge,
              firing[i].ptr[j].max_charge, firing[i].ptr[j].targ_id,
              firing[i].ptr[j].noise, firing[i].ptr[j].height);
}
}
*/
    printf("\n Hit return to continue...");
    G_gets(buf);

}
