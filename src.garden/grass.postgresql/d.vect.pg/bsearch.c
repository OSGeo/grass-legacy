#include <stdio.h>
/* binary search. taken from Knuth: Sorting ans Searching, p 407 */
bin_search (key, n, start, cmp)
	int *key;           /* key to search for. Passed to cmp() */
	int n;               /* number of entries in table */
	int *start;          /* current starting point in table */
	int (*cmp)();        /* cmp(key, i): compare key to table entry i
			      *  returns -1 if key < table entry i
			      *           1 if key > table entry i
			      *           0 if key == table entry i
			      */
{
	int dir, cur, low, high, mark;

	cur = *start;
	if (cur < 0 || cur >= n)
		cur = n/2;
B1:
	low=0;
	high=n-1;
	goto B3;

B2:
	if (high < low) return 0; /* no match */
	cur = (low+high)/2;

B3:
	dir = cmp(key, cur);
	if (dir < 0) goto B4;
	if (dir > 0) goto B5;
	/* match. */
	mark = cur;

	/* find first item in table which matches */
	while (--cur >= 0)
		if (cmp(key, cur) != 0)
			break;
	cur++;
	*start = cur;

	/* find last item in table which matches */
	cur = mark;
	while (++cur < n)
		if (cmp (key, cur) != 0)
			break;
	/* return number of table entries which match */
	return cur - *start;

B4:
	high = cur - 1;
	goto B2;

B5:
	low = cur + 1;
	goto B2;
}

	


