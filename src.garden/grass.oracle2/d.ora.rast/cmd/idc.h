/* 
 * sm.h
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
#ifndef __SM_H__
#define  __SM_H__

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#define SHMKEY		((key_t) 7890)
#define SEMKEY_IDC	((key_t) 7891)
#define CHUNK_PERM	0644

typedef struct {
	int id;
	int sem;
	char *ptr;
} shm_t;

typedef struct {
	int pid;
	unsigned cnt;
} idc_t;

typedef struct {
	long cpu_usec;
	long t_usec;
	unsigned cnt;
} usage_t;

#define IDC_SIZE	sizeof(idc_t)


/* proto */
char *shm_create(shm_t * shm);
void shm_free(shm_t *shm);
int sem_lock(int sem, int index);
int sem_trylock(int sem, int index);
int sem_unlock(int sem, int index);
int getusage(usage_t *use);

#endif

