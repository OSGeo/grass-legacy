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



#ifndef CCFOREST_H
#define CCFOREST_H


#include <iostream.h>
#include <assert.h>

#include <ami.h>


#define DEBUG_CCFOREST if(0)

template<class T>
class keyvalue {
private:
  T key, value;
public:
  keyvalue() : key(-1), value(-1) {};
  keyvalue(T vk, T vv) : key(vk), value(vv) {};

  T getPriority() const { return key; };
  T getValue() const { return value; };
  T src() const { return key; };
  T dst() const { return value; };

  keyvalue operator =(const keyvalue &that) {
    key = that.key;
    value = that.value;
    return *this;
  };
  int operator != (const keyvalue &e2) const {
    return (key != e2.key) || (value != e2.value);
  }
  int operator == (const keyvalue &e2) const {
    return (value == e2.value) && (key == e2.key);
  }
  int operator > (const keyvalue &e2) const {
    return (key > e2.key) || (key == e2.key && value > e2.value);
  }
  int operator >= (const keyvalue &e2) const {
    return (key > e2.key) || (key == e2.key && value >= e2.value);
  }
  int operator < (const keyvalue &e2) const {
    return (key < e2.key) || (key == e2.key && value < e2.value);
  }
  int operator <= (const keyvalue &e2) const {
    return (key <= e2.key) || (key == e2.key && value <= e2.value);
  }

  friend ostream& operator<<(ostream& s, const keyvalue &p) {
    return s << "(" << p.key << "," << p.value << ")";
  }

  static int qscompare(const void *a, const void *b) {
	keyvalue<T> *x = (keyvalue<T>*)a;
	keyvalue<T> *y = (keyvalue<T>*)b;
	return compare(*x, *y);
  }

  static int compare(const keyvalue<T> &x, const keyvalue<T> &y) {
	return  (x < y ? -1 : (x > y ? 1 : 0));
  }

};


/* laura: used in sort (instead of <); checkit  */
template<class T>
class keyCmpKeyvalueType {
public:
  static int compare(const keyvalue<T>  &a, const keyvalue<T>  &b) {
	if (a.getPriority() < b.getPriority()) return -1;
	if (a.getPriority() > b.getPriority()) return 1;
	return 0;
  }
};



/* ---------------------------------------------------------------------- */
/* laura: used in sort instead of valueCmp; checkit */
template<class T>
class dstCmpKeyvalueType {
public: 
  static int compare (const keyvalue<T> &a, const keyvalue<T> &b) {
    if(a.dst() < b.dst()) return -1;
    if(a.dst() > b.dst()) return 1;
    
    if(a.src() < b.src()) return -1;
    if(a.src() > b.src()) return 1;
    
    return 0;
  }
};



template<class T> class ccforest;

template<class T> 
class ccforest {
  /* class cckeyvalue : public keyvalue<T> {}; */
  typedef keyvalue<T> ccedge;
  typedef keyvalue<T> cckeyvalue;
private:  
  AMI_STREAM<ccedge> *edgeStream;
  AMI_STREAM<cckeyvalue> *rootStream;
 
  void findAllRoots(int depth=0);
  int rootCycles;
  ccforest<T> *superTree;
  void removeDuplicates(T src, T parent,
			EMPQueueAdaptive<cckeyvalue,T> &pq);
  int foundAllRoots;
  cckeyvalue savedRoot;
  int savedRootValid;
public:
  ccforest();
  ~ccforest();
  void insert(const T& i, const T& j); /* insert edge (i,j) */
  T findNextRoot(const T& i);	/* find root where i >= prev i */
  void printRootStream();
  void printEdgeStream();
  int size();
};


#endif /* CCFOREST_H */

