#define EXTERN extern

#include "inter.h"

get_sediment(i) int i;

{

    int k;
    int size;
    int minus;
    char answer;
    char get_answer();
    int seg_num;

    seg_num = i+1;

    if (i == 0)
    {
        num_sizes = 3;

        while(1)
        {
            V_clear();

            V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");

            V_line(4,"Number of sediment sizes:                     default from GRASS:");
            V_line(5,"  Recommended maximum:  10 sizes");

            V_ques(&num_sizes,'i',4,41,5);
            V_const(&num_sizes,'i',4,67,4);

            V_intrpt_ok();
            if (V_call() == 0)
            {
                fprintf(stderr,"existed with interrupt key (^C)...\n");
                exit(7);
            }

            if ((num_sizes > 0) && (num_sizes <= 10))
                break;
            else
            {
                fprintf(stderr,"Error -- must choose a number of sediment sizes");
                fprintf(stderr," between 1 and 10 inclusive\n");
                sleep(2);
            }
        }

        if (num_sizes == 3)
        {
            fprintf(stdout,"Want to use sediment sizes and percentages from GRASS? [y/n] ");
            answer = get_answer();
            if (answer == 'y')
                sed_default = 1;
        }

        sed = (SED *)G_calloc(2*num_sizes,sizeof(SED));
        for (size=0; size<=(2*num_sizes-1); size++)
        {
            sed[size].sed_size = 0.;
            sed[size].per_finer = 0.;
        }

        V_clear();

        V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");
        V_line(4,"Segment number: ");

        V_const(&seg_num,'i',4,19,5);

        V_line(6,"Number of sediment sizes chosen:");

        V_const(&num_sizes,'i',6,34,4);

        V_float_accuracy(4);

        if (sed_default)
        {
            sed[0].sed_size = .002;
            sed[1].sed_size = .05;
            sed[2].sed_size = 2.0;

            for (size=0; size<3; size++)
            {
                V_line((9+(size)*2),"Sediment size (mm):              from GRASS estimate:");
                V_ques(&sed[size].sed_size,'f',(9+(size*2)),24,8);
                V_const(&sed[size].sed_size,'f',(9+(size)*2),55,8);

            }
        }
        else
        {
            minus = 0;

            for (size=0; size<num_sizes; size++)
            {
                V_line((9+(size-minus)*2),"Sediment size (mm):");
                V_ques(&sed[size].sed_size,'f',(9+(size-minus)*2),24,8);

                if (((size+1)/5)*5 == (size+1))
                {
                    minus += 5;
                    V_intrpt_ok();
                    if (V_call() == 0)
                    {
                        fprintf(stderr,"exited with interrupt key (^C)...\n");
                        exit(7);
                    }
                    V_clear();

                    V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");
                    V_line(4,"Segment number: ");

                    V_const(&seg_num,'i',4,19,5);

                    V_line(6,"Number of sediment sizes chosen:");

                    V_const(&num_sizes,'i',6,34,4);

                    V_float_accuracy(4);
                }
            }
        }

        if (((num_sizes/5)*5 != num_sizes) || (sed_default))
        {
            V_intrpt_ok();
            if (V_call() == 0)
            {
                fprintf(stderr,"exited with interrupt key (^C)...\n");
                exit(7);
            }
        }

        for (size=0; size<num_sizes; size++)
            sed[size+num_sizes].sed_size = sed[size].sed_size;
    }

    if (unit_type[i] == 2)
    {

        V_clear();

        V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");
        V_line(4,"Segment number: ");

        V_const(&seg_num,'i',4,19,5);

        V_line(7,"Number of sediment sizes chosen:");

        V_const(&num_sizes,'i',7,34,4);

        V_float_accuracy(4);

        if (sed_default)
        {

            sed[0].per_finer = finer1[0] + finer1[1];
            sed[1].per_finer = finer2[0] + finer2[1];
            sed[2].per_finer = 1.0;

            for (size=0; size<=2; size++)
            {
                V_line((9+(size)*2),"Sediment size:              mm");
                V_const(&sed[size].sed_size,'f',(9+(size*2)),19,8);

                V_line((10+(size)*2),"  Percent finer:               from GRASS estimate:");
                V_ques(&sed[size].per_finer,'f',(10+(size)*2),19,8);
                V_const(&sed[size].per_finer,'f',(10+(size)*2),53,8);
            }
        }
        else
        {
            minus = 0;

            for (size=0; size<num_sizes; size++)
            {
                V_line((9+(size-minus)*2),"Sediment size:              mm");
                V_const(&sed[size].sed_size,'f',(9+(size-minus)*2),19,8);

                V_line((10+(size-minus)*2),"  Percent finer:");
                V_ques(&sed[size].per_finer,'f',(10+(size-minus)*2),19,8);

                if (((size+1)/5)*5 == (size+1))
                {

                    minus += 5;
                    V_intrpt_ok();
                    if (V_call() == 0)
                    {
                        fprintf(stderr,"exited with interrupt key (^C)...\n");
                        exit(7);
                    }

                    V_clear();

                    V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");
                    V_line(4,"Segment number: ");

                    V_const(&seg_num,'i',4,19,5);

                    V_line(7,"Number of sediment sizes chosen:");

                    V_const(&num_sizes,'i',7,34,4);

                    V_float_accuracy(4);
                }
            }
        }

        if (((num_sizes/5)*5 != num_sizes) || (sed_default))
        {
            V_intrpt_ok();
            if (V_call() == 0)
            {
                fprintf(stderr,"exited with interrupt key (^C)...\n");
                exit(7);
            }
        }
    }
    else
    {
        for (k=0; k<=1; k++)
        {
            if (k == 0)
                strcpy(plane,"left ");
            else
                strcpy(plane,"right");
    

            V_clear();

            V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");
            V_line(4,"Segment number: ");
            V_line(5,"Plane: ");

            V_const(&seg_num,'i',4,19,5);
            V_const(plane,'s',5,19,6);

            V_line(7,"Number of sediment sizes chosen:");

            V_const(&num_sizes,'i',7,34,4);

            V_float_accuracy(4);

            if (sed_default)
            {

                sed[0+k*num_sizes].per_finer = finer1[k];
                sed[1+k*num_sizes].per_finer = finer2[k];
                sed[2+k*num_sizes].per_finer = 1.0;

                for (size=0; size<=2; size++)
                {
                    V_line((9+(size)*2),"Sediment size:              mm");
                    V_const(&sed[size+k*num_sizes].sed_size,'f',(9+(size*2)),19,8);

                    V_line((10+(size)*2),"  Percent finer:               from GRASS estimate:");
                    V_ques(&sed[size+k*num_sizes].per_finer,'f',(10+(size)*2),19,8);
                    V_const(&sed[size+k*num_sizes].per_finer,'f',(10+(size)*2),53,8);
                }
            }
            else
            {
                minus = 0;

                for (size=0; size<=(num_sizes-1); size++)
                {
                    V_line((9+(size-minus)*2),"Sediment size:              mm");
                    V_const(&sed[size+k*num_sizes].sed_size,'f',(9+(size-minus)*2),19,8);

                    V_line((10+(size-minus)*2),"  Percent finer:");
                    V_ques(&sed[size+k*num_sizes].per_finer,'f',(10+(size-minus)*2),19,8);

                    if (((size+1)/5)*5 == (size+1))
                    {

                        minus += 5;
                        V_intrpt_ok();
                        if (V_call() == 0)
                        {
                            fprintf(stderr,"exited with interrupt key (^C)...\n");
                            exit(7);
                        }

                        V_clear();

                        V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");
                        V_line(4,"Segment number: ");
                        V_line(5,"Plane: ");

                        V_const(&seg_num,'i',4,19,5);
                        V_const(plane,'s',5,19,6);

                        V_line(7,"Number of sediment sizes chosen:");

                        V_const(&num_sizes,'i',7,34,4);

                        V_float_accuracy(4);
                    }
                }
            }

            if (((num_sizes/5)*5 != num_sizes) || (sed_default))
            {
                V_intrpt_ok();
                if (V_call() == 0)
                {
                    fprintf(stderr,"exited with interrupt key (^C)...\n");
                    exit(7);
                }
            }

        }
    }


    V_clear();

    V_line(2,"      EROSION/SEDIMENT CHARACTERISTICS");
    V_line(4,"Segment number: ");
    V_line(6,"Plane:            left");

    V_const(&seg_num,'i',4,19,5);

    if (stats_flag)
    {
        V_line(7,"  Overland Flow Detachment Coefficient:");
        V_line(8,"      from GRASS soils:");
        V_line(9,"  Soil Plasticity Index:      from GRASS soils:");

        V_ques(&Dof[0],'f',7,41,11);
        V_const(&Dof[0],'f',8,25,11);
        V_ques(&plasticity[0],'i',9,26,3);
        V_const(&plasticity[0],'i',9,49,3);

        V_line(11,"Plane:            right");

        V_line(12,"  Overland Flow Detachment Coefficient:");
        V_line(13,"      from GRASS soils:");
        V_line(14,"  Soil Plasticity Index:      from GRASS soils:");

        V_ques(&Dof[1],'f',12,41,11);
        V_const(&Dof[1],'f',13,25,11);
        V_ques(&plasticity[1],'i',14,26,3);
        V_const(&plasticity[1],'i',14,49,3);

        V_line(16,"Channel Flow Detachment Coefficient:");
        V_line(17,"      from average of planes:");
        V_line(18,"Plasticity for Channel:");
        V_line(19,"      from average of planes:");

        ChDof = (Dof[0] + Dof[1]) / 2.;
        V_ques(&ChDof,'f',16,39,11);
        V_const(&ChDof,'f',17,31,11);
        Chplast = (plasticity[0] + plasticity[1]) / 2.;
        V_ques(&Chplast,'f',18,29,11);
        V_const(&Chplast,'f',19,31,11);
    }
    else
    {
        V_line(7,"  Overland Flow Detachment Coefficient:");
        V_line(8,"  Soil Plasticity Index:");

        V_ques(&Dof[0],'f',7,41,11);
        V_ques(&plasticity[0],'i',8,26,3);

        V_line(10,"Plane:            right");

        V_line(11,"  Overland Flow Detachment Coefficient:");
        V_line(12,"  Soil Plasticity Index:");

        V_ques(&Dof[1],'f',11,41,11);
        V_ques(&plasticity[1],'i',12,26,3);

        V_line(15,"Channel Flow Detachment Coefficient:");
        V_line(16,"Plasticity for Channel:");

        V_ques(&ChDof,'f',15,39,11);
        V_ques(&Chplast,'f',16,39,11);
    }

    V_intrpt_ok();
    if (V_call() == 0)
    {
        fprintf(stderr,"exited with interrupt key (^C)...\n");
        exit(7);
    }
}
