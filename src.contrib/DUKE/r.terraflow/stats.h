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



#ifndef STATS_H
#define STATS_H

/* $Id$ */

#include <sys/types.h>

#include <fstream>
#include <iostream>
using namespace std;

#include <ami.h>




int noclobberFile(char *);

class statsRecorder : public ofstream {
private:
  Rtimer tm;
  void *bss;
public:
  statsRecorder(char *fname);
  ~statsRecorder() { 
	this->flush(); 
  }
  char *freeMem(char *);
  long freeMem();
  char *timestamp();
  void timestamp(const char *s);
  void comment(const char *s, const int verbose=1);
  void comment(const char *s1, const char *s2);
  void comment(const int n);
  void recordTime(const char *label, long secs);
  void recordTime(const char *label, Rtimer rt);
  void recordLength(const char *label, off_t len, int siz=0, char *sname=NULL);
  template<class T> void recordLength(const char *label, AMI_STREAM<T> *str) {
	recordLength(label, str->stream_len(), sizeof(T), str->sprint());
  }
};

char *formatNumber(char *buf, off_t val);

#endif
