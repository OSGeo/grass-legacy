
#define EXTERN extern

#include "inter.h"

get_rain(i) int i;

{

    char answer;
    char get_answer();
    int k;
    int rain_count;
    int minus;
    int seg_num;

    seg_num = i+1;

    if (rain_flag)
        return;

    if (unit_type[i] == 2)
    {

        NRAIN1 = 1;

        while(1)
        {
            V_clear();

            V_line(2,"           RAINFALL CHARACTERISTICS");

            V_line(4,"Segment number: ");

            V_const(&seg_num,'i',4,19,5);

            V_line(7,"Number of rainfall increments:");
            V_line(8,"  (recommended minimum 4)");

            V_ques(&NRAIN1,'i',7,32,5);

            V_intrpt_ok();
            if (V_call() == 0)
            {
                fprintf(stderr,"exited with interrupt key (^C)...\n");
                exit(7);
            }

            if (NRAIN1 >= 1)
                break;
            else
                fprintf(stderr,"Error -- must have at least one increment\n");
        }

        rain1 = (RAIN *)G_calloc(NRAIN1,sizeof(RAIN));
        for (rain_count=0; rain_count<NRAIN1; rain_count++)
        {
            rain1[rain_count].intensity = 0.;
            rain1[rain_count].end_time = 0;
        }

        V_clear();

        V_line(2,"           RAINFALL CHARACTERISTICS");

        V_line(4,"Segment number: ");

        V_const(&seg_num,'i',4,19,5);

        minus = 0;

        for (rain_count=0; rain_count<NRAIN1; rain_count++)
        {
            V_line(7+(rain_count-minus)*2,"Rainfall intensity (in/hr):");
            V_line(8+(rain_count-minus)*2,"  Ending time of increment (minutes):");

            V_ques(&rain1[rain_count].intensity,'f',7+(rain_count-minus)*2,39,6);
            V_ques(&rain1[rain_count].end_time,'i',8+(rain_count-minus)*2,39,5);

            if (((rain_count+1)/7) * 7 == (rain_count+1))
            {

                minus += 7;

                V_intrpt_ok();
                if (V_call() == 0)
                {
                    fprintf(stderr,"exited with interrupt key (^C)...\n");
                    exit(7);
                }

                V_clear();
    
                V_line(2,"           RAINFALL CHARACTERISTICS");

                V_line(4,"Segment number: ");

                V_const(&seg_num,'i',4,19,5);

            }

            if ((NRAIN1/7)*7 != NRAIN1)
            {
                V_intrpt_ok();
                if (V_call() == 0)
                {
                    fprintf(stderr,"exited with interrupt key (^C)...\n");
                    exit(7);
                }
            }
        }

        if (i == 0)
        {
            fprintf(stdout,"Want to use same values for all units? [y/n] ");
            answer = get_answer();
            if (answer == 'y')
            {
                rain_flag = 1;
                return;
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
    
        if (k==0) NRAIN1 = 1;
        if (k==1) NRAIN2 = 1;

        while(1)
        {
            V_clear();

            V_line(2,"           RAINFALL CHARACTERISTICS");

            V_line(4,"Segment number: ");
            V_line(5,"Plane: ");

            V_const(&seg_num,'i',4,19,5);
            V_const(plane,'s',5,19,6);

            V_line(7,"Number of rainfall increments:");
            V_line(8,"  (recommended minimum 4)");

            if (k==0) V_ques(&NRAIN1,'i',7,32,5);
            if (k==1) V_ques(&NRAIN2,'i',7,32,5);

            V_intrpt_ok();
            if (V_call() == 0)
            {
                fprintf(stderr,"exited with interrupt key (^C)...\n");
                exit(7);
            }

            if ((k==0) && (NRAIN1 >= 1))
                break;
            else if ((k==1) && (NRAIN2 >=1))
                break;
            else
                fprintf(stderr,"Error -- must have at least one increment\n");
        }

        if (k==0)
        {
            rain1 = (RAIN *)G_calloc(NRAIN1,sizeof(RAIN));
            for (rain_count=0; rain_count<=(NRAIN1-1); rain_count++)
            {
                rain1[rain_count].intensity = 0.;
                rain1[rain_count].end_time = 0;
            }
        }
        if (k==1)
        {
            rain2 = (RAIN *)G_calloc(NRAIN2,sizeof(RAIN));
            for (rain_count=0; rain_count<=(NRAIN2-1); rain_count++)
            {
                rain2[rain_count].intensity = 0.;
                rain2[rain_count].end_time = 0;
            }
        }

        V_clear();

        V_line(2,"           RAINFALL CHARACTERISTICS");

        V_line(4,"Segment number: ");
        V_line(5,"Plane: ");

        V_const(&seg_num,'i',4,19,5);
        V_const(plane,'s',5,19,6);

        minus = 0;

        if (k==0)
        {
            for (rain_count=0; rain_count<=NRAIN1-1; rain_count++)
            {
                V_line(7+(rain_count-minus)*2,"Rainfall intensity (in/hr):");
                V_line(8+(rain_count-minus)*2,"  Ending time of increment (minutes):");

                V_ques(&rain1[rain_count].intensity,'f',7+(rain_count-minus)*2,39,6);
                V_ques(&rain1[rain_count].end_time,'i',8+(rain_count-minus)*2,39,5);

                if (((rain_count+1)/7) * 7 == (rain_count+1))
                {
    
                    minus += 7;

                    V_intrpt_ok();
                    if (V_call() == 0)
                    {
                        fprintf(stderr,"exited with interrupt key (^C)...\n");
                        exit(7);
                    }

                    V_clear();
        
                    V_line(2,"           RAINFALL CHARACTERISTICS");

                    V_line(4,"Segment number: ");
                    V_line(5,"Plane: ");

                    V_const(&seg_num,'i',4,19,5);
                    V_const(plane,'s',5,19,6);

                }
            }

            if ((NRAIN1/7)*7 != NRAIN1)
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
            for (rain_count=0; rain_count<=NRAIN2-1; rain_count++)
            {
                V_line(7+(rain_count-minus)*2,"Rainfall intensity (in/hr):");
                V_line(8+(rain_count-minus)*2,"  Ending time of increment (minutes):");

                V_ques(&rain2[rain_count].intensity,'f',7+(rain_count-minus)*2,39,6);
                V_ques(&rain2[rain_count].end_time,'i',8+(rain_count-minus)*2,39,5);

                if (((rain_count+1)/7) * 7 == (rain_count+1))
                {
    
                    minus += 7;

                    V_intrpt_ok();
                    if (V_call() == 0)
                    {
                        fprintf(stderr,"exited with interrupt key (^C)...\n");
                        exit(7);
                    }

                    V_clear();
        
                    V_line(2,"           RAINFALL CHARACTERISTICS");

                    V_line(4,"Segment number: ");
                    V_line(5,"Plane: ");

                    V_const(&seg_num,'i',4,19,5);
                    V_const(plane,'s',5,19,6);

                }
            }

            if ((NRAIN2/7)*7 != NRAIN2)
            {
                V_intrpt_ok();
                if (V_call() == 0)
                {
                    fprintf(stderr,"exited with interrupt key (^C)...\n");
                    exit(7);
                }
            }
        }

        if ((k == 0) && (i == 0))
        {
            fprintf(stdout,"Want to use same values for all units? [y/n] ");
            answer = get_answer();
            if (answer == 'y')
            {
                rain_flag = 1;
                return;
            }
        }
    }
    }
}
