#include <sys/utsname.h>
struct utsname attname;

main()
{
    uname (&attname);
    fprintf (stdout,"%s\n", attname.nodename);
}
