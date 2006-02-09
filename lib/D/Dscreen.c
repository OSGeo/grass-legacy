#include <grass/D.h>
int Dscreen()
{
    Dclearscreen();
    Dnew ("full_screen", 0.0, 100.0, 0.0, 100.0);
    Dchoose ("full_screen");

    return 0;
}
