/* %W% %G% */
#include "mapmask.h"

vask3()
{
    int i;
    extern double *Ux,*Uy;

    V_line( 6,"                                        x");
    V_line( 7,"                                       x x");
    V_line( 8,"                                      x   x");
    V_line( 9,"                                     x     x");
    V_line(10,"                                    x       x");
    V_line(11,"                                   x         x");
    V_line(12,"                                  x           x");
    V_line(13,"                                 x             x");
    V_line(14,"                                x               x");
    V_line(15,"                               x                 x");
    V_line(16,"                              x                   x");
    V_line(17,"                             x                     x");
    V_line(18,"                            x                       x");
    V_line(19,"                           x                         x");
    V_line(20,"                          x x x x x x x x x x x x x x x");

    for(i = 0; i < 3; i++)
    {
	Ux[i] = 0.0;
	Uy[i] = 0.0;
    }

    V_ques(&Ux[0],'d',6,43,10);
    V_ques(&Ux[1],'d',19,56,10);
    V_ques(&Ux[2],'d',19,12,10);
    V_ques(&Uy[0],'d',7,43,10);
    V_ques(&Uy[1],'d',20,56,10);
    V_ques(&Uy[2],'d',20,12,10);

    V_call();

}
