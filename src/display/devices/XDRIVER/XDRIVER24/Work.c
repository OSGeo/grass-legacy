
#include "includes.h"
#include "driverlib.h"

#define LOOP_PER_SERVICE 15

int
Has_work(void)
{
    return 1;
}

int
Work_stream(void)
{
    return ConnectionNumber(dpy);
}

void
Do_work(int opened)
{
    static int cmd_loop_count;

    if (opened)
    {
        if(--cmd_loop_count <= 0) 
        {
            Service_Xevent(opened);  /* take care of any events */
            cmd_loop_count = LOOP_PER_SERVICE;
        }
    }
    else
    {
        Service_Xevent(opened); 
        XNoOp(dpy);            /* see if X is still running */
        cmd_loop_count = 0;
    }
}

