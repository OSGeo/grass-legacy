/* %W% %G% */
#include "mapmask.h"

vask7()
{
    int i;
    extern double *Ux,*Uy;

    V_line( 6,"                                         x");
    V_line( 7,"                                       x   x");    
    V_line( 8,"                                     x       x"); 
    V_line( 9,"                                   x           x"); 
    V_line(10,"                                 x               x"); 
    V_line(11,"                               x                   x");
    V_line(12,"                             x                       x");
    V_line(13,"                             x                       x");
    V_line(14,"                             x                       x");
    V_line(15,"                             x                       x");
    V_line(16,"                              x                     x");
    V_line(17,"                               x                   x");
    V_line(18,"                                x                 x");
    V_line(19,"                                 x               x");
    V_line(20,"                                  xxxxxxxxxxxxxxx");

    for(i = 0; i < 7; i++)
    {
	Ux[i] = 0.0;
	Uy[i] = 0.0;
    }

    V_ques(&Ux[0],'d',6,44,10);
    V_ques(&Ux[1],'d',11,56,10);
    V_ques(&Ux[2],'d',15,56,10);
    V_ques(&Ux[3],'d',19,51,10);
    V_ques(&Ux[4],'d',19,22,10);
    V_ques(&Ux[5],'d',15,17,10);
    V_ques(&Ux[6],'d',11,17,10);
    V_ques(&Uy[0],'d',7,44,10);
    V_ques(&Uy[1],'d',12,56,10);
    V_ques(&Uy[2],'d',16,56,10);
    V_ques(&Uy[3],'d',20,51,10);
    V_ques(&Uy[4],'d',20,22,10);
    V_ques(&Uy[5],'d',16,17,10);
    V_ques(&Uy[6],'d',12,17,10);

    V_call();
}
