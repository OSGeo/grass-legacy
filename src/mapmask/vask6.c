/* %W% %G% */
#include "mapmask.h"

vask6()
{
    int i;
    extern double *Ux,*Uy;

    V_line( 6,"                                 xxxxxxxxxxxxxxxx");
    V_line( 7,"                                x                x");
    V_line( 8,"                               x                  x");
    V_line( 9,"                              x                    x");
    V_line(10,"                             x                      x");
    V_line(11,"                            x                        x");
    V_line(12,"                           x                          x");
    V_line(13,"                          x                            x");
    V_line(14,"                           x                          x");
    V_line(15,"                            x                        x");
    V_line(16,"                             x                      x");
    V_line(17,"                              x                    x");
    V_line(18,"                               x                  x");
    V_line(19,"                                x                x");
    V_line(20,"                                 xxxxxxxxxxxxxxxx");

    for(i = 0; i < 6; i++)
    {
	Ux[i] = 0.0;
	Uy[i] = 0.0;
    }

    V_ques(&Ux[0],'d',6,18,10);
    V_ques(&Ux[1],'d',6,51,10);
    V_ques(&Ux[2],'d',13,56,10);
    V_ques(&Ux[3],'d',19,51,10);
    V_ques(&Ux[4],'d',19,18,10);
    V_ques(&Ux[5],'d',13,12,10);
    V_ques(&Uy[0],'d',7,18,10);
    V_ques(&Uy[1],'d',7,51,10);
    V_ques(&Uy[2],'d',14,56,10);
    V_ques(&Uy[3],'d',20,51,10);
    V_ques(&Uy[4],'d',20,18,10);
    V_ques(&Uy[5],'d',14,12,10);

    V_call();
}
