
#define EXTERN extern

#include "inter.h"

get_surface(i) int i;

{

    int k;
    int seg_num;

    seg_num = i+1;

    for (k=0; k<=1; k++)
    {
        V_clear();

        V_line(2,"                SURFACE CHARACTERISTICS");
        V_line(4,"Segment number: ");
        V_line(5,"Plane:");

        if (k == 0)
            strcpy(plane,"left ");
        else
            strcpy(plane,"right");

        V_const(&seg_num,'i',4,19,5);
        V_const(plane,'s',5,19,5);


        if (stats_flag)
        {
            V_line(7,"Percent canopy:                 from GRASS cover file:");
            V_line(8,"Percent ground:                 from GRASS cover file:");
            V_line(9,"Percent impervious:             from GRASS cover file:");

            V_float_accuracy(3);

/* stored in statistics collection file from GRASS as a decimal.  ARMSED
   expects input as a percent. */

            cover[k*3] *= 100;
            cover[k*3+1] *= 100;
            cover[k*3+2] *= 100;

            V_ques(&cover[k*3],'f',7,21,11);
            V_const(&cover[k*3],'f',7,56,11);
            V_ques(&cover[k*3+1],'f',8,21,11);
            V_const(&cover[k*3+1],'f',8,56,11);
            V_ques(&cover[k*3+2],'f',9,21,11);
            V_const(&cover[k*3+2],'f',9,56,11);

            V_line(11,"*Potential depth of rainfall storage on canopy cover:");
            V_line(12,"*Potential depth of rainfall storage on ground cover:");
            V_line(13,"    *(in inches) -- a reasonable estimate is 0.1 inches");

            V_ques(&VC[k],'f',11,55,6);
            V_ques(&VG[k],'f',12,55,6);

            if (depres_flag)
            {
                V_line(15,"Fraction of non-contributing flow area:");
                V_line(16,"    from GRASS estimate:");
                V_line(17,"Maximum roughness coefficient for overland flow:");
                V_line(18,"    from GRASS estimate:");

                V_ques(&depres[k],'d',15,55,6);
                V_const(&depres[k],'d',16,26,6);
                V_ques(&ADW[k],'d',17,55,11);
                V_const(&ADW[k],'d',18,26,11);
            }
            else
            {

                V_line(15,"Fraction of non-contributing flow area:");
                V_line(16,"Maximum roughness coefficient for overland flow:");
                V_line(17,"    from GRASS estimate:");

                V_ques(&VC[k],'f',11,55,6);
                V_ques(&VG[k],'f',12,55,6);
                V_ques(&depres[k],'d',15,55,6);
                V_ques(&ADW[k],'d',16,55,11);
                V_const(&ADW[k],'d',17,26,11);
            }

        }
        else
        {
            V_line(7,"Percent canopy:");
            V_line(8,"Percent ground:");
            V_line(9,"Percent impervious:");

            V_float_accuracy(3);
            V_ques(&cover[k*3],'f',7,21,11);
            V_ques(&cover[k*3+1],'f',8,21,11);
            V_ques(&cover[k*3+2],'f',9,21,11);

            V_line(11,"*Potential depth of rainfall storage on canopy cover:");
            V_line(12,"*Potential depth of rainfall storage on ground cover:");
            V_line(13,"    *(in inches) -- a reasonable estimate is 0.1 inches");
            V_line(15,"Fraction of non-contributing flow area:");
            V_line(16,"Maximum roughness coefficient for overland flow:");

            V_ques(&VC[k],'f',11,55,6);
            V_ques(&VG[k],'f',12,55,6);
            V_ques(&depres[k],'f',15,55,6);
            V_ques(&ADW[k],'d',16,55,11);

        }

        V_intrpt_ok();
        if (V_call() == 0)
        {
            fprintf(stderr,"exited with interrupt key (^C)...\n");
            exit(7);
        }

/* k goes from 0 to 1 here */
    }

    if (wat_flag)
    {

        V_clear();

        V_line(2,"                SURFACE CHARACTERISTICS");
        V_line(4,"Segment number: ");

        V_const(&seg_num,'i',4,19,5);

        V_float_accuracy(3);

        V_line(6,"Manning's n for channel:");
        V_line(8,"  estimates:  .02 for smooth channels");
        V_line(9,"              .035 for channels with short grass or rocks");
        V_line(10,"              .10 for very weedy, brushy channels");

        V_ques(&Mann_n,'f',6,26,6);

        V_intrpt_ok();
        if (V_call() == 0)
        {
            fprintf(stderr,"exited with interrupt key (^C)...\n");
            exit(7);
        }
    }

}
