/*********** 
From pfiland@mindspring.com Sat Jan 18 07:58:16 2003
Path: newsmaster1.news.pas.earthlink.net!stamper.news.pas.earthlink.net!stamper.news.atl.earthlink.net!harp.news.atl.earthlink.net!not-for-mail
From: pete <pfiland@mindspring.com>
Newsgroups: comp.lang.c,comp.programming
Subject: Re: micro optimizing shellsort, Re: Use of Pointer variable..
Date: Sat, 18 Jan 2003 09:49:04 -0500
Organization: PF
Lines: 264
Message-ID: <3E296960.3A6A@mindspring.com>
References: <ec573ad3.0301170136.17cb9678@posting.google.com> <b08mts$otf$1@murdoch.hpl.hp.com> <3E2800BB.1A76@mindspring.com> <b0922c$40h$1@sunnews.cern.ch> <3E294348.5040706@combase.com>
Reply-To: pfiland@mindspring.com
NNTP-Posting-Host: a8.c1.a8.95
Mime-Version: 1.0
Content-Type: text/plain; charset=us-ascii
Content-Transfer-Encoding: 7bit
X-Server-Date: 18 Jan 2003 14:48:58 GMT
X-Mailer: Mozilla 3.04Gold (WinNT; I)
Xref: stamper.news.pas.earthlink.net comp.lang.c:528165 comp.programming:154559
X-Received-Date: Sat, 18 Jan 2003 06:48:59 PST (newsmaster1.news.pas.earthlink.net)

Al Bowers wrote:

> I put the C Standard function qsort into the mix with two compilers
> targeting the same platform. I was surprised at the results. qsort
> with MS Visual 6.0 was far worse and gcc 3.2's qsort was the best.

Microsoft's qsort uses a looping style similar
to that of qsort in K&R2 section 4.10 Recursion.
I posted an example of such a function in the 
    Re: signed and unsigned ?
thread in comp.lang.c and cmp.std.c
It's a very short version of qsort and it illustrated a concern
that I had at that time.


gcc uses Sedgewick style inner loops, 
and a median of 3 selection, to choose the pivot,
and finishes with an insertion sort, so that the quicksort
partions don't have to be partitioned all the way down to one element.

On my system, I find that I do better, if instead of 
finishing with insertion sort, I sort partitions smaller than 5
with an if else tree.

If you like to race qsorts, and I do, here's the latest version
of my magnum opus, qisort:
********/

/* BEGIN qisort.c */

#include <limits.h>
#include <stddef.h>

void
qisort(void *, size_t, size_t, 
    int (*)(const void *, const void *));

/* 
** Use qisort the same way as the standard library function, qsort.
** qisort is guaranteed not to "go quadratic" 
** for worst case behavior.
*/
/* 
** Introspective, nonrecursive, Median of Three, Quicksort,
** biased for forward and reverse ordered arrays.
** Partitions of less than five elements,
** are sorted by an "if else" tree.
** A worst case of at least 23 elements, is required
** before the heapsort routine kicks in.
** The heapsort routine 
** limits worst case performance to O N log N.
*/
#define SWAP(A, B)                                          \
{                                                           \
    p1 = (A);                                               \
    p2 = (B);                                               \
    end = p2 + size;                                        \
    do {                                                    \
        swap  = *p1;                                        \
        *p1++ = *p2;                                        \
        *p2++ = swap;                                       \
    } while (p2 != end);                                    \
}
#define SORT_2(A, B)                                        \
    if (compar((A), (B)) > 0) {                             \
        SWAP((A), (B));                                     \
    };
#define SORT_3(A, B, C)                                     \
    if (compar((A), (B)) > 0) {                             \
        if (compar((C), (B)) > 0) {                         \
            SWAP((A), (B));                                 \
            SORT_2((B), (C));                               \
        } else {                                            \
            SWAP((A), (C));                                 \
        }                                                   \
    } else {                                                \
        if (compar((B), (C)) > 0) {                         \
            SWAP((B), (C));                                 \
            SORT_2((A), (B));                               \
        }                                                   \
    }
#define SORT_4(A, B, C, D)                                  \
    if (compar((B), (D)) > 0) {                             \
        SWAP((C), (D));                                     \
        SWAP((B), (C));                                     \
        SORT_2((A), (B));                                   \
    } else {                                                \
        SORT_2((C), (D));                                   \
    }

void qisort(void *base, size_t nmemb, size_t size, 
           int (*compar)(const void *, const void *))
{
    unsigned char *middle, *last, *left, *right, *center;
    size_t bytes, two_memb, four_memb, odd_mask;
    struct {
        size_t bytes;
        void *base;
    } stack[CHAR_BIT * sizeof nmemb * 2], *stack_ptr, *stack_limit;
    unsigned char swap, *p1, *p2, *end;

    if (nmemb > 4) {
        odd_mask  = ((size ^ (size - 1)) >> 1) + 1;
        two_memb  = size     << 1;
        four_memb = two_memb << 1;
        stack -> bytes = nmemb * size;
        stack -> base  = base;
        stack_ptr   = stack + 1;
        stack_limit = stack + 5;
        nmemb >>= 3;
        while (nmemb > (unsigned char)-1 >> 1) {
            stack_limit += CHAR_BIT << 1;
            nmemb      >>= CHAR_BIT;
        }
        while (nmemb) {
            stack_limit += 2;
            nmemb      >>= 1;
        }
        do {
            --stack_ptr;
            bytes = stack_ptr -> bytes;
            if (bytes > four_memb) {
                left = stack_ptr -> base;
                if (stack_ptr != stack_limit) {
                    last = middle = center = left;
                    right = last += bytes - size;
                    center += (bytes & odd_mask 
                        ? bytes - size : bytes) >> 1;
                    SWAP(center, left);
                    middle += size;
                    SORT_3(middle, left, last);
                    do {
                        middle += size;
                    } while (compar(left, middle) > 0);
                    do {
                        last -= size;
                    } while (compar(last, left) > 0);
                    while (last > middle) {
                        SWAP(middle, last);
                        do {
                            middle += size;
                        } while (compar(left, middle) > 0);
                        do {
                            last -= size;
                        } while (compar(last, left) > 0);
                    }
                    SWAP(left, last);
                    if (last > center) {
                        stack_ptr -> bytes = right - last;
                        stack_ptr -> base  = size  + last;
                        ++stack_ptr;
                        stack_ptr -> bytes = last - left;
                        stack_ptr -> base  = left;
                        ++stack_ptr;
                    } else {
                        stack_ptr -> bytes = last - left;
                        ++stack_ptr;
                        stack_ptr -> bytes = right - last;
                        stack_ptr -> base  = size  + last;
                        ++stack_ptr;
                    }
                } else {
                    unsigned char *cbase, *parent, *child;
                    ptrdiff_t offset;

                    right = cbase = left;
                    right += bytes - size;
                    left += (bytes & odd_mask 
                        ? bytes - size : bytes) >> 1;
                    do {
                        offset = left - cbase;
                        child = parent = left -= size;
                        while (right - parent > offset) {
                            child += offset;
                            if (compar(child + size, child) > 0) {
                                child += size;
                            }
                            if (compar(child, parent) > 0) {
                                SWAP(parent, child);
                                offset = child - cbase + size;
                                parent = child;
                            } else {
                                break;
                            }
                        }
                        if (right - parent == offset 
                            && compar(right, parent) > 0) {
                            SWAP(parent, right);
                        }
                    } while (left != cbase);
                    do {
                        offset = size;
                        child = parent = cbase;
                        while (right - child > offset) {
                            child += offset;
                            if (compar(child + size, child) > 0) {
                                child += size;
                            }
                            SWAP(parent, child);
                            offset = child - cbase + size;
                            parent = child;
                        }
                        if (child != right) {
                            SWAP(child, right);
                            if (right - child != offset 
                                && (child != right - size 
                                || (offset & odd_mask))
                                &&  child != cbase) {
                                offset = child - cbase + size;
                                parent -= (offset & odd_mask 
                                    ? size + offset : offset) >> 1;
                                while (compar(child, parent) > 0) {
                                    SWAP(parent, child);
                                    offset = parent - cbase + size;
                                    child = parent;
                                    parent -= (offset & odd_mask 
                                        ? size + offset 
                                        : offset) >> 1;
                                }
                            }
                        }
                        right -= size;
                    } while (right != cbase);
                }
            } else {
                if (bytes != size) {
                    left = base = stack_ptr -> base;
                    left += size;
                    if (bytes == two_memb) {
                        SORT_2(base, left);
                    } else {
                        middle = left + size;
                        SORT_3(base, left, middle);
                        if (bytes == four_memb) {
                            right = middle + size;
                            SORT_4(base, left, middle, right);
                        }
                    }
                }
            }
        } while (stack_ptr != stack);
    } else {
        if (nmemb > 1) {
            left = base;
            left += size;
            if (nmemb == 2) {
                SORT_2(base, left);
            } else {
                middle = left + size;
                SORT_3(base, left, middle);
                if (nmemb == 4) {
                    right = middle + size;
                    SORT_4(base, left, middle, right);
                }
            }
        }
    }
}

/* END qisort.c */

/****

-- 
pete
****/

