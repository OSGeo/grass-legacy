/*
 * IDC.C
 */
/* Copyright (C) 1994-1999 NSA srl - Ennio Pozzetti (pozzetti@nsa.it)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are not permitted without the Author written permission.
 * 
 * This library is free for non-commercial use as long as
 * the following conditions are aheared to.  The following conditions
 * apply to all code found in this distribution.
 * The documentation included with this distribution is covered by the 
 * same copyright terms.
 *
 * Copyright remains NSA srl, and as such any Copyright notices in
 * the code are not to be removed.
 * If this package is used in a product, NSA srl should be given attribution
 * as the author of the parts of the library used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted for non commercial use provided that 
 * the following conditions are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    "This product includes software written by
 *     NSA srl - Ennio Pozzetti (pozzetti@nsa.it).
 *
 * THIS SOFTWARE IS PROVIDED BY NSA SRL ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * The licence and distribution terms for any available version or
 * derivative of this code cannot be changed.  i.e. this code cannot simply be
 * copied and put under another distribution licence
 * [including the GNU Public Licence.]
 */
#include <stdio.h>
#include "idc.h"

char *shm_create(shm_t * shm)
{
    shm->id = shmget(SHMKEY, IDC_SIZE, CHUNK_PERM | IPC_CREAT);
    shm->ptr = (char *)shmat(shm->id, (char *)0, 0);
    return(shm->ptr);
}

void shm_free(shm_t *shm)
{
    shmdt(shm->ptr);
}

#include <limits.h>
#include <sys/time.h>


#define IDC_TIC_SEC     11000000.0
#define IDC_TIC_USEC    11.0


#define UTVDIFF(ts, te) \
    (1000000 * ((te).tv_sec - (ts).tv_sec) + ((te).tv_usec - (ts).tv_usec))

int getusage(usage_t *use)
{
    static shm_t shm;
    static attach = 0, start = 1;
    static idc_t *idc;
    static unsigned cnt_start, cnt_stop;
    static struct timeval ts, te;
    unsigned t_usec;
    unsigned cpu_usec;

    if(attach == 0) {
        if((idc = (idc_t *)shm_create(&shm)) == NULL)
            return(-1);
        attach = 1;
    }

    if(start) {
        start = 0;
        gettimeofday(&ts, 0);
        cnt_start = idc->cnt;
    }
    else {
        cnt_stop = idc->cnt;
        gettimeofday(&te, 0);
        use->cnt = (cnt_start <= cnt_stop) ? cnt_stop - cnt_start
                                     : UINT_MAX - cnt_start + cnt_stop;

        use->t_usec =  UTVDIFF(ts, te);
        use->cpu_usec = use->t_usec - (use->cnt / IDC_TIC_USEC);
        start = 1;
    }
    return(1);
}

