#define EXTERN extern

#include "inter.h"

get_data(i) int i;

{
    double sqrt();
    double pow();
    int pl_num;
    double K2[2];

    if (wat_flag)
    {
        for (pl_num=0; pl_num<=1; pl_num++)
        {
            if (K[pl_num] != 0)
            {
                if (K[pl_num] > .6)
                    K[pl_num] = .6;
                if (K[pl_num] < .07)
                    K[pl_num] = .07;
                K2[pl_num] = (double)(K[pl_num] * K[pl_num] * K[pl_num]);
                K2[pl_num] = sqrt(sqrt(K2[pl_num]));
                ADW[pl_num] = (double)4220 * ((double)1. / K2[pl_num]);
            }
            if (plasticity[pl_num] != 0)
                Tc[pl_num] = .0034 * pow((double)plasticity[pl_num],(double).84);
        }
    }

    get_intro(i);
    get_geometric(i);
    get_soil(i);
    get_sediment(i);
    get_surface(i);
    get_rain(i);

}
