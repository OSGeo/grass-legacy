#define EXTERN extern

#include "inter.h"

get_geometric(i) int i;

{

    double sqrt();
    int k;
    int pl_num;
    int seg_num;

    seg_num = i+1;

    V_clear();

    V_line(2,"                GEOMETRIC CHARACTERISTICS");
    V_line(4,"Segment number: ");
    V_line(6,"Plane: left");
    V_const(&seg_num,'i',4,19,5);


    if (stats_flag)
    {
        V_line(7,"  Overland flow length (ft):              from GRASS division file:             ");
        V_line(8,"  Overland slope:                         from GRASS slopes file:");

        V_float_accuracy(3);
        V_ques(&flow[0],'d',7,30,11);
        V_const(&flow[0],'d',7,69,11);
        V_ques(&olslope[0],'d',8,30,11);
        V_const(&olslope[0],'d',8,69,11);

        V_line(10,"Plane: right");

        V_line(11,"  Overland flow length (ft):              from GRASS division file:");
        V_line(12,"  Overland slope:                         from GRASS slopes file:");

        V_ques(&flow[1],'d',11,30,11);
        V_const(&flow[1],'d',11,69,11);
        V_ques(&olslope[1],'d',12,30,11);
        V_const(&olslope[1],'d',12,65,11);

    }
    else
    {
        V_line(7,"  Overland flow length (ft):");
        V_line(8,"  Overland slope:");

        V_ques(&flow[0],'d',7,30,11);
        V_float_accuracy(3);
        V_ques(&olslope[0],'d',8,30,11);

        V_line(10,"Plane: right");

        V_line(11,"  Overland flow length (ft):");
        V_line(12,"  Overland slope:");

        V_ques(&flow[1],'d',11,30,11);
        V_ques(&olslope[1],'d',12,30,11);
    }

    V_intrpt_ok();
    if (V_call() == 0)
    {
        fprintf(stderr,"exited with interrupt key (^C)...\n");
        exit(7);
    }

    for (pl_num=0; pl_num<=1; pl_num++)
    {
        if (olslope[pl_num] != 0)
        {
            z[pl_num] = 1 / olslope[pl_num];
            b[pl_num] = .5;
        }
    }

    if (z[0] != 0 && z[1] != 0)
    {
        a[0] = sqrt(2/(z[0]+z[1])) * (sqrt(1+pow(z[0],2)) +
          sqrt(1+pow(z[1],2)));

        a[1] = sqrt(2*(z[0]+z[1]));
    }

    V_clear();

    V_line(2,"                GEOMETRIC CHARACTERISTICS");
    V_line(4,"Segment number: ");

    V_const(&seg_num,'i',4,19,5);

    V_float_accuracy(4);

    if (stats_flag)
    {

        V_line(6,"Channel length (ft):                      from GRASS streams file:");
        V_line(7,"Channel slope:                            from GRASS slopes file:");

        V_ques(&chlength,'d',6,30,11);
        V_const(&chlength,'d',6,69,11);
        V_ques(&chslope,'d',7,30,11);
        V_const(&chslope,'d',7,69,11);

    }
    else
    {

        V_line(6,"Channel length (ft):");
        V_line(7,"Channel slope:");

        V_ques(&chlength,'d',6,30,11);
        V_ques(&chslope,'d',7,30,11);

    }

    V_line(9,"The below parameters are from the equations");
    V_line(10," p = a1*Aexp(b1)  and  T = a2*Aexp(b2)");
    V_line(11,"where p is wetted perimeter and T is top width of channel");

    if (stats_flag)
    {

        V_line(13,"a1:              value calculated from overland slope*:");
        V_line(14,"b1:              set value*:");
        V_line(15,"a2:              value calculated from overland slope*:");
        V_line(16,"b2:              set value*:");
        V_line(18,"                    *assuming triangular channel");

        V_const(&a[0],'d',13,57,11);
        V_const(&b[0],'d',14,57,11);
        V_const(&a[1],'d',15,57,11);
        V_const(&b[1],'d',16,57,11);
    }
    else
    {

        V_line(13,"a1:");
        V_line(14,"b1:");
        V_line(15,"a2:");
        V_line(16,"b2:");
    }

    V_ques(&a[0],'d',13,5,11);
    V_ques(&b[0],'d',14,5,11);
    V_ques(&a[1],'d',15,5,11);
    V_ques(&b[1],'d',16,5,11);

    V_intrpt_ok();
    if (V_call() == 0)
    {
        fprintf(stderr,"exited with interrupt key (^C)...\n");
        exit(7);
    }

}
