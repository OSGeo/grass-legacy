#ifndef USE_RAND

#include <sys/types.h>

#ifdef __CYGWIN__
#define drand48() rand()/32767.0
#define srand48(sv) (srand((unsigned)(sv)))
#else
extern long drand48();
extern void srand48();
#endif 

extern time_t time();

long 
make_rand (void)
{
    return lrand48();
}

void 
init_rand (void)
{
    srand48( (long) time( (time_t *) 0) );
}

#else

static long 
labs (int n)
{
    return n < 0 ? (-n) : n;
}

long 
make_rand (void)
{
    return (labs(rand() + (rand() << 16)));
}

void 
init_rand (void)
{
    srand(getpid());
}

#endif
