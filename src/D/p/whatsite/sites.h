#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN struct sites
{
	double n, e;
	long offset ;
} sites[1024] ;   /* This should be an allocation */

EXTERN int nsites ;

EXTERN double mark_size ;
