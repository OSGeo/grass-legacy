#include "config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif
#include "gis.h"

/*******************************************************************************
*       sleep_ltp       (see manual, Internal Functions)                <in>   *
*        Sleeps for fractional seconds                                         *
*                                                                              *
*        parameters:  double tm      fractional time to sleep (.001 sec res)   *
*                                                                              *
*        Returns zero upon normal end-of-job (EOJ).                            *
*                      j.dabritz  7/30/90                                      *
*******************************************************************************/

long sleep_ltp( double tm)
{	
	double finish;
	double check;

	time_ltp(&check); 
	for(finish = check + tm;  check <= finish; time_ltp(&check))
		sleep(0);

	return(0);
}

/*******************************************************************************
*       time_ltp        (see manual, Internal Functions)                <in>   *
*        Sets a double to the time in fractional seconds.                      *
*                                                                              *
*        parameters:  double *time    pointer to double to be set.             *
*                                                                              *
*        Returns zero upon normal end-of-job (EOJ).                            *
*                      j.dabritz  7/30/90                                      *
*******************************************************************************/

#ifdef HAVE_GETTIMEOFDAY

int time_ltp( double *time)
{
	struct timeval tstruct;

	gettimeofday (&tstruct, NULL);
	*time = tstruct.tv_sec + tstruct.tv_usec / 1000000.0;
	return(0);
}

#else

#ifdef HAVE_FTIME

int time_ltp( double *time)
{
	struct timeb tstruct;

	ftime(&tstruct);
	*time = tstruct.time + tstruct.millitm / 1000.0;
	return(0);
}

#else

#ifdef HAVE_TIME

int time_ltp(double *time)
{
	time_t tloc;

	time(&tloc);  /** get time (whole seconds) **/
	*time = tloc;  /** convert to a double **/
	return(0);
}

#endif /* HAVE_TIME */
#endif /* HAVE_FTIME */
#endif /* HAVE_GETTIMEOFDAY */
