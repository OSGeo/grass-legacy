/* %W% %G% */
#include "mapmask.h"

vask4()
{

    int i;
    extern double *Ux,*Uy;

    V_line( 7,"                         xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
    V_line( 8,"                         x                            x");
    V_line( 9,"                         x                            x");
    V_line(10,"                         x                            x");
    V_line(11,"                         x                            x");
    V_line(12,"                         x                            x");
    V_line(13,"                         x                            x");
    V_line(14,"                         x                            x");
    V_line(15,"                         x                            x");
    V_line(16,"                         xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

    for(i = 0; i < 4; i++)
    {
	Ux[i] = 0.0;
	Uy[i] = 0.0;
    }

    V_ques(&Ux[0],'d',7,12,10);
    V_ques(&Ux[1],'d',7,56,10);
    V_ques(&Ux[2],'d',15,56,10);
    V_ques(&Ux[3],'d',15,12,10);
    V_ques(&Uy[0],'d',8,12,10);
    V_ques(&Uy[1],'d',8,56,10);
    V_ques(&Uy[2],'d',16,56,10);
    V_ques(&Uy[3],'d',16,12,10);

    V_call();
}
