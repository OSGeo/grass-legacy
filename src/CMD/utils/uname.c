#include <sys/utsname.h>
struct utsname attname;

main()
{
    uname (&attname);
    printf ("%s\n", attname.nodename);
}
