/*C
 * Original project: Lars Arge, Jeff Chase, Pat Halpin, Laura Toma, Dean
 *		     Urban, Jeff Vitter, Rajiv Wickremesinghe 1999
 * 
 * GRASS Implementation: Lars Arge, Helena Mitasova, Laura Toma 2002
 *
 * Copyright (c) 2002 Duke University -- Laura Toma 
 *
 * Copyright (c) 1999-2001 Duke University --
 * Laura Toma and Rajiv Wickremesinghe
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Duke University
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE TRUSTEES AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE TRUSTEES OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *C*/



#ifndef RTIMER_H
#define RTIMER_H

/* $Id$ */

#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

typedef struct {
  struct rusage rut1, rut2;
  struct timeval tv1, tv2;
} Rtimer;

#define rt_start(rt)							\
  if((getrusage(RUSAGE_SELF, &rt.rut1) < 0)		\
	 || (gettimeofday(&(rt.tv1), NULL) < 0)) {	\
	perror("rusage/gettimeofday");				\
	exit(1);									\
  }


/* doesn't really stop, just updates endtimes */
#define rt_stop(rt)								\
  if((getrusage(RUSAGE_SELF, &rt.rut2) < 0)		\
	 || (gettimeofday(&(rt.tv2), NULL) < 0)) {	\
        perror("rusage/gettimeofday");			\
        exit(1);								\
  }

/* not required to be called, but makes values print as 0. 
   obviously a hack */
#define rt_zero(rt) bzero(&(rt),sizeof(Rtimer));
	

#define rt_u_useconds(rt)							\
	(((double)rt.rut2.ru_utime.tv_usec +			\
	  (double)rt.rut2.ru_utime.tv_sec*1000000)		\
	 - ((double)rt.rut1.ru_utime.tv_usec +			\
		(double)rt.rut1.ru_utime.tv_sec*1000000))

#define rt_s_useconds(rt)							\
	 (((double)rt.rut2.ru_stime.tv_usec +			\
	   (double)rt.rut2.ru_stime.tv_sec*1000000)		\
	  - ((double)rt.rut1.ru_stime.tv_usec +			\
		 (double)rt.rut1.ru_stime.tv_sec*1000000))

#define rt_w_useconds(rt)							\
	 (((double)rt.tv2.tv_usec +			\
	   (double)rt.tv2.tv_sec*1000000)		\
	  - ((double)rt.tv1.tv_usec +			\
		 (double)rt.tv1.tv_sec*1000000))

#define rt_seconds(rt) (rt_w_useconds(rt)/1000000)

#define rt_sprint(buf, rt) rt_sprint_safe(buf,rt)

char * rt_sprint_safe(char *buf, Rtimer rt);



#endif /* RTIMER_H */
