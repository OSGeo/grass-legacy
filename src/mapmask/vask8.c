/* %W% %G% */
#include "mapmask.h"

vask8()
{
    int i;
    extern double *Ux,*Uy;

    V_line( 5,"                                  xxxxxxxxxxxx");
    V_line( 6,"                                x              x");
    V_line( 7,"                              x                  x");
    V_line( 8,"                            x                      x");
    V_line( 9,"                          x                          x");
    V_line(10,"                        x                              x");
    V_line(11,"                        x                              x");
    V_line(12,"                        x                              x");
    V_line(13,"                        x                              x");
    V_line(14,"                        x                              x");
    V_line(15,"                        x                              x");
    V_line(16,"                          x                          x");
    V_line(17,"                            x                      x");
    V_line(18,"                              x                  x");
    V_line(19,"                                x              x");
    V_line(20,"                                  xxxxxxxxxxxx");

    for(i = 0; i < 8; i++)
    {
	Ux[i] = 0.0;
	Uy[i] = 0.0;
    }

    V_ques(&Ux[0],'d',5,18,10);
    V_ques(&Ux[1],'d',5,49,10);
    V_ques(&Ux[2],'d',10,58,10);
    V_ques(&Ux[3],'d',15,58,10);
    V_ques(&Ux[4],'d',19,49,10);
    V_ques(&Ux[5],'d',19,18,10);
    V_ques(&Ux[6],'d',15,10,10);
    V_ques(&Ux[7],'d',10,10,10);
    V_ques(&Uy[0],'d',6,18,10);
    V_ques(&Uy[1],'d',6,49,10);
    V_ques(&Uy[2],'d',11,58,10);
    V_ques(&Uy[3],'d',16,58,10);
    V_ques(&Uy[4],'d',20,49,10);
    V_ques(&Uy[5],'d',20,18,10);
    V_ques(&Uy[6],'d',16,10,10);
    V_ques(&Uy[7],'d',11,10,10);

    V_call();
}
