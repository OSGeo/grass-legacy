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


#ifndef __UNION_FIND
#define __UNION_FIND

#include <assert.h>
#include <iostream.h>
#include <stdlib.h>
#include <string.h>


/* initial range guesstimate */
#define UNION_INITIAL_SIZE 2000


/* maintains a collection of disjoint dynamic sets; elements are in
the range 1..maxsize-1 and range is increased dynamically; if element x
is not in the set then parent[x] = 0;
*/

template<class T>
class unionFind { 
private: 
  T* parent; 
  T*  rank; 
  T maxsize;
  
public:
  unionFind();
  ~unionFind();
  
  /* return true if element x is in the structure */
  inline bool inSet(T x);

  void print();

  /* create a new set whose only member (and representative) is x;
	 since x is disjoint we require x not be already in the set; */
  inline void makeSet(T x);
  
  /* unites the dynamic sets that contain x and y; */
  inline void makeUnion(T x, T y);
  
  /* returns the representative of the set containing x */
  inline T findSet(T x);
  
  /* returns main memory usage if the max nb of makeset calls is n */
  inline size_t mmusage(T n);
};


/************************************************************/
template<class T>
unionFind<T>::unionFind() {
  maxsize = UNION_INITIAL_SIZE;
  /*  parent = new (long)[maxsize]; */
  parent = (T*)calloc(maxsize, sizeof(T));
  assert(parent);
  /*  rank = new (long)[maxsize]; */
  rank = (T*)calloc(maxsize, sizeof(T));
  assert(rank);
}

/************************************************************/
template<class T>
unionFind<T>::~unionFind() {
  /* if (parent) delete [] parent; 
	 if (rank) delete [] rank; 
  */
  if (parent) free(parent);
  if (rank) free(rank);
}

/************************************************************/
/* return true if element x is in the structure */
template<class T>
inline bool 
unionFind<T>::inSet(T x) {
  if (x > 0 && x < maxsize && parent[x] > 0) {
    return true;
  } else {
    return false;
  }
}



/************************************************************/
template<class T>
void 
unionFind<T>::print() {
  for (T i=0; i< maxsize; i++) {
    if (parent[i] == 0) cout << "x ";
    else cout << parent[i] << " ";
  }
  cout << "\n";
}

/************************************************************/
/* 
create a new set whose only member (and representative) is x; since x
is disjoint we require x not be already in the set; */
template<class T>
inline void
unionFind<T>::makeSet(T x) {
  /* cout << "makeSet " << x << "\n"; print(); */
  assert(x > 0);
  if (x >= maxsize) {
    /* reallocate parent */
    cout << "UnionFind::makeSet: reallocate double " << maxsize << "\n";
    parent = (T*)realloc(parent, 2*maxsize*sizeof(T));
    assert(parent);
    memset(parent + maxsize, 0, maxsize*sizeof(T));
    /*reallocate rank */
    rank = (T*)realloc(rank, 2*maxsize*sizeof(T));
    assert(rank);
    memset(rank + maxsize, 0, maxsize*sizeof(T));
    /*update maxsize */
    maxsize *= 2;
  }  
  /*since x is disjoint we require x not be already in the set; should
    relax this..*/
  assert(!inSet(x));
  parent[x] = x;
  rank[x] = 0;
}

/************************************************************/
/* returns the representative of the set containing x */
template<class T>
inline T
unionFind<T>::findSet(T x) {
  /* valid entry */
  assert(inSet(x));
  if (parent[x] != x) {
    /* path compression heuristic */
    parent[x] = findSet(parent[x]);
  }
  /* parent[x] must be a root */
  assert(parent[parent[x]] == parent[x]);
  return parent[x];
}


/************************************************************/
/* unites the dynamic sets that contain x and y; */
template<class T>
inline void 
unionFind<T>::makeUnion(T x, T y) {
  assert(inSet(x) && inSet(y));
  T setx = findSet(x);
  T sety = findSet(y);
  if (setx == sety) return;

  /* union by rank heuristic */
  assert(inSet(x) && inSet(y));
  if (rank[setx] > rank[sety]) {
    /* hook sety onto setx */
    parent[sety] = setx;
  } else {
    /* hook setx onto sety */
    parent[setx] = sety;
    /* if equal increase rank */
    if (sety == sety) {
      rank[sety]++;
    }
  }
  /* this does not have side effects.. */
  assert(findSet(x) == findSet(y));
}


/************************************************************/
/* returns main memory usage if the max nb of makeset calls is n */
template<class T>
inline size_t 
unionFind<T>::mmusage(T n) {
  if (n < UNION_INITIAL_SIZE) n = UNION_INITIAL_SIZE;
  return (n * sizeof(T));
}


#endif

