#include "vask.h"
#include "imagery.h"
#include <stdlib.h>

int I_v_exec()
{
    V_intrpt_ok();
    if (!V_call()) exit(0);
    return 0;
}
