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



#include <sys/types.h>
#include <sys/mman.h>
#include <ctype.h>

#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
#include <ostream>
#else
#include <ostream.h>
#endif

#include <iostream>
using namespace std;

#include "common.h"


/* globals */

statsRecorder *stats = NULL;
userOptions *opt = NULL;
struct  Cell_head *region = NULL; 
dimension_type nrows = 0, ncols = 0;


size_t
parse_number(const char *s) {
  size_t n, mult=1;
  int len = strlen(s);
  if(isalpha(s[len-1])) {
    switch(s[len-1]) {
    case 'M':
      mult = 1 << 20;
      break;
    case 'K':
      mult = 1 << 10;
      break;
    default:
      cerr << "bad number format: " << s << endl;
      exit(-1);
      break;
    }
    /* s[len-1] = '\0'; not needed, as it will stop at first invalid char */
  }
  n = atol(s);
  return n * mult;
}




/* ---------------------------------------------------------------------- */
/* is anybody using this?? DELETE ! */

#ifdef USE_LARGEMEM

void *LargeMemory::ptr[LM_HIST];
size_t LargeMemory::len[LM_HIST];
int LargeMemory::next = 0;

#ifndef MAP_ANON
#define MAP_ANON 0
#endif

#ifdef __alpha
#undef MAP_FAILED
#define MAP_FAILED (caddr_t)-1L
#endif


void *
LargeMemory::alloc(size_t leng) {
  assert(next < LM_HIST);
  void *p = mmap(0, leng, PROT_READ|PROT_WRITE, MAP_ANON, -1, 0);
  if(p == MAP_FAILED) {
	perror("mmap");
	exit(1);
  }
  len[next] = leng;
  ptr[next] = p;
  next++;
  if(stats) {
	char buf[BUFSIZ], buf2[32];
	sprintf(buf, "allocated large memory: %s 0x%lX", 
			formatNumber(buf2, leng), (unsigned long)p);
	stats->comment(buf);
  }
  return p;
}

void
LargeMemory::free(void *p) {
  int z;
  int i;
  for(i=next-1; i>=0; i--) {
	if(ptr[i] == p) break;
  }
  assert(i<next && i>=0); /* must have been allocated before */
  
#if (defined sun && defined sparc)
  z = munmap((caddr_t)p, len[i]);
#else
  z = munmap(p, len[i]);
#endif
  if(z < 0) {
	perror("munmap");
  }

  if(stats) {
	char buf[BUFSIZ], buf2[32];
	sprintf(buf, "freed large memory: %s 0x%lX", 
			formatNumber(buf2, len[i]), (unsigned long)p);
	stats->comment(buf);
  }

  next--;
  if(next) {
	ptr[i] = ptr[next];
	len[i] = len[next];
  }
}

#endif /*  USE_LARGEMEM */


