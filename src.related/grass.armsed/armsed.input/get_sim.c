
#define EXTERN extern

#include "inter.h"

get_sim()

{

    NCON = 0;
    DTIM = 0;
    FTIM = 0;

    V_clear();

    V_line(2,"           SIMULATION INPUTS");

    V_line(4,"Simulation title:");

    V_const(sim_title,'s',4,19,10);

    V_line(8,"Incremental simulation times (minutes):");
    V_line(9,"Total simulation time (minutes):");
    V_line(10,"Number of connections to other basins:");

    V_ques(&DTIM,'i',8,41,5);
    V_ques(&FTIM,'i',9,41,5);
    V_ques(&NCON,'i',10,41,5);

    V_intrpt_ok();
    if (V_call() == 0)
    {
        fprintf(stderr,"exited with interrupt key (^C)...\n");
        exit(7);
    }

}
