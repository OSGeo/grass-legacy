#include <grass/config.h>

#ifdef HAVE_CURSES_H
#include <grass/gis.h>
#include <grass/vask.h>
#include <grass/imagery.h>
#include <stdlib.h>

int I_v_exec()
{
    V_intrpt_ok();
    if (!V_call())
        exit(EXIT_SUCCESS);

    return 0;
}

#endif /* HAVE_CURSES_H */
