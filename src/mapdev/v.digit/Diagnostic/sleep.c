/*******************************************************************************
*       sleep_ltp       (see manual, Internal Functions)                <in>   *
*        Sleeps for fractional seconds                                         *
*                                                                              *
*        parameters:  double tm      fractional time to sleep (.001 sec res)   *
*                                                                              *
*        Returns zero upon normal end-of-job (EOJ).                            *
*                      j.dabritz  7/30/90                                      *
*******************************************************************************/

#include "ginput.h"

long sleep_ltp(tm)
	double tm;
{	
	double finish;
	double check;
#ifdef SV32
	long i;

	finish = tm + 0.9;
	i = finish;
	sleep(i);
#else
	time_ltp(&check); 
	for(finish = check + tm;  check <= finish; time_ltp(&check))
		sleep(0);
#endif

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

#ifdef SV32

#include <sys/types.h>

time_t time();

long time_ltp(tme)
	double *tme;
{	
	long tloc;

	time(&tloc);  /** get time (whole seconds) **/

	*tme = tloc;  /** convert to a double **/

	return(0);
}
#else

#include <sys/types.h>
#include <sys/timeb.h>

long time_ltp(time)
	double *time;
{	
	struct timeb tstruct;


	ftime(&tstruct);

	*time = tstruct.time + tstruct.millitm / 1000.0;
	
	return(0);
}
#endif
