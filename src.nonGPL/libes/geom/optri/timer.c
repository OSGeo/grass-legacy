#include <sys/types.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/param.h>

double get_user_time (void)
     /* Returns user seconds elapsed since start of process. */
{
  /* struct tms is defined in sys/times.h (see man times); tms values are
     in "clock ticks per second", ie, HZ, which is defined in sys/param.h */
  struct tms buffer;
  (void) times (&buffer);
  return (((double) buffer.tms_utime) / ((double) 60));
}

double get_time_of_day (void)

/* returns GMT time in seconds.hundrets_of_second */

{
  struct timeval tp;
  struct timezone tzp;

  (void) gettimeofday (&tp, &tzp);

  return ((double) (tp.tv_sec % 86400)) +
         ((double) ((long) (tp.tv_usec / ((double) (1000000.0 / 60))))
	  / 100.0);
}

