#ifndef USE_RAND

#include <sys/types.h>

extern long lrand48();
extern void srand48();
extern time_t time();

long
make_rand()
{
    return lrand48();
}

void
init_rand()
{
    srand48( (long) time( (time_t *) 0) );
}

#else

static long labs(n)
{
    return n < 0 ? (-n) : n;
}

long
make_rand()
{
    return (labs(rand() + (rand() << 16)));
}

void
init_rand()
{
    srand(getpid());
}

#endif
