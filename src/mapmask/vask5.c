/* %W% %G% */
#include "mapmask.h"

vask5()
{
    int i;
    extern double *Ux,*Uy;

    V_line( 6,"                               x");
    V_line( 7,"                              x x");
    V_line( 8,"                             x   x");
    V_line( 9,"                            x     x");
    V_line(10,"                           x       x");
    V_line(11,"                          x         x");
    V_line(12,"                         x           x");
    V_line(13,"                        x             x");
    V_line(14,"                       x               x");
    V_line(15,"                      x                 x");
    V_line(16,"                       x               x");
    V_line(17,"                        x             x");
    V_line(18,"                         x           x");
    V_line(19,"                          x         x");
    V_line(20,"                           xxxxxxxxx");

    for(i = 0; i < 5; i++)
    {
	Ux[i] = 0.0;
	Uy[i] = 0.0;
    }

    V_ques(&Ux[0],'d',6,34,10);
    V_ques(&Ux[1],'d',15,42,10);
    V_ques(&Ux[2],'d',19,37,10);
    V_ques(&Ux[3],'d',19,14,10);
    V_ques(&Ux[4],'d',15,10,10);
    V_ques(&Uy[0],'d',7,34,10);
    V_ques(&Uy[1],'d',16,42,10);
    V_ques(&Uy[2],'d',20,37,10);
    V_ques(&Uy[3],'d',20,14,10);
    V_ques(&Uy[4],'d',16,10,10);

    V_call();
}
