#include <sys/types.h>
#include <sys/timeb.h>
 
main ()
{
        struct timeb tstruct;
 
 
        ftime(&tstruct);
 
        return(0);
}
