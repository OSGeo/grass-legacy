
#define EXTERN extern

#include "inter.h"

get_intro(i) int i;

{

    char answer[4];
    int seg_num;

    seg_num = i+1;

    Temp = 0.;

    while(1)
    {
        strcpy(answer,"");

        V_clear();

        V_line(2,"               INTRODUCTORY INPUTS");

        V_line(4,"Segment number: ");

        V_const(&seg_num,'i',4,19,5);

        V_line(6,"Unit identification:");
        V_line(7,"Results printed? [yes/no]:");
        V_line(9,"Soil temperature (degrees F):");
        V_ques(unit_ident,'s',6,40,12);
        V_ques(answer,'s',7,40,4);
        V_ques(&Temp,'f',9,40,7);

        V_intrpt_ok();
        if (V_call() == 0)
        {
            fprintf(stderr,"exited with interrupt key (^C)...\n");
            exit(7);
        }

        if (strncmp(answer,"yes",strlen("yes")) == 0)
        {
            print[i] = 1;
            break;
        }
        else if (strncmp(answer,"no",strlen("no")) == 0)
        {
            print[i] = -1;
            break;
        }
        else
        {
            fprintf(stderr,"Please answer yes or no about having results printed\n");
            sleep(2);
        }
    }

}
