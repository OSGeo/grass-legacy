#define EXTERN extern

#include "inter.h"

get_soil(i) int i;

{

    int k;
    int seg_num;

    seg_num = i+1;

    for (k=0; k<=1; k++)
    {
        if (k == 0)
            strcpy(plane,"left ");
        else
            strcpy(plane,"right");
    
        V_clear();

        V_line(2,"                  SOIL CHARACTERISTICS");
        V_line(4,"Segment number: ");
        V_line(5,"Plane: ");

        V_const(&seg_num,'i',4,19,5);
        V_const(plane,'s',5,19,6);

        V_float_accuracy(3);

        if (stats_flag)
        {

            V_line(7,"Hydraulic Conductivity (in/hr):              from GRASS soils file:");
            V_line(8,"Capillary Suction (in):                      from GRASS soils file:");
            V_line(9,"Soil Porosity:                               from GRASS soils file:");

            V_ques(&Kw[k],'f',7,33,11);
            V_const(&Kw[k],'f',7,68,11);
            V_ques(&Yc[k],'f',8,33,11);
            V_const(&Yc[k],'f',8,68,11);
            V_ques(&porosity[k],'f',9,33,11);
            V_const(&porosity[k],'f',9,68,11);

        }
        else
        {
            V_line(7,"Hydraulic Conductivity (in/hr):");
            V_line(8,"Capillary Suction (in):");
            V_line(9,"Soil Porosity:");

            V_ques(&Kw[k],'f',7,33,11);
            V_ques(&Yc[k],'f',8,33,11);
            V_ques(&porosity[k],'f',9,33,11);

        }

        V_line(11,"Initial Soil Saturation:");
        V_line(12,"  Reasonable estimates:");
        V_line(13,"    wet conditions:  0.8 or greater");
        V_line(14,"    average:         0.5");
        V_line(15,"    very dry:        0.15");
        V_line(17,"Final Soil Saturation:");
        V_line(18,"   A reasonable estimate is 1.0");

        V_ques(&Si[k],'f',11,27,6);
        V_ques(&Sw[k],'f',17,27,6);

        V_intrpt_ok();
        if (V_call() == 0)
        {
            fprintf(stderr,"exited with interrupt key (^C)...\n");
            exit(7);
        }
    }
    V_clear();

    V_line(2,"                  SOIL CHARACTERISTICS");
    V_line(4,"Segment number: ");

    V_const(&seg_num,'i',4,19,5);

    V_float_accuracy(3);

    V_line(6,"FOR CHANNEL:");

    V_line(8,"Hydraulic Conductivity (in/hr):              avg of 2 planes:");
    V_line(10,"Capillary Suction (in):                      avg of 2 planes:");
    V_line(12,"Soil Porosity:                               avg of 2 planes:");

    chKw = (Kw[0] + Kw[1]) / 2;
    chYc = (Yc[0] + Yc[1]) / 2;
    chporos = (porosity[0] + porosity[1]) / 2;

    V_ques(&chKw,'f',8,33,11);
    V_const(&chKw,'f',8,65,11);
    V_ques(&chYc,'f',10,33,11);
    V_const(&chYc,'f',10,65,11);
    V_ques(&chporos,'f',12,33,11);
    V_const(&chporos,'f',12,65,11);

    V_line(14,"Initial Soil Saturation:                    avg of 2 planes:");
    V_line(16,"Final Soil Saturation:                      avg of 2 planes:");

    chSi = (Si[0] + Si[1]) / 2;
    chSw = (Sw[0] + Sw[1]) / 2;

    V_ques(&chSi,'f',14,37,6);
    V_const(&chSi,'f',14,65,5);
    V_ques(&chSw,'f',16,37,6);
    V_const(&chSw,'f',16,65,5);

    V_intrpt_ok();
    if (V_call() == 0)
    {
        fprintf(stderr,"exited with interrupt key (^C)...\n");
        exit(7);
    }
}
