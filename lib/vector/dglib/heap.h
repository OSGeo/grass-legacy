/* LIBDGL -- a Directed Graph Library implementation
 * Copyright (C) 2002 Roberto Micarelli
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/* best view tabstop=4
 */

#ifndef _GN_HEAP_H_
#define _GN_HEAP_H_

__BEGIN_DECLS

typedef union _gnHeapData
{
	void * 			pv;
	int				n;
	unsigned int 	un;
	long			l;
	unsigned long 	ul;

} gnHeapData_u;


typedef struct _gnHeapNode
{
	long 			key;
	gnHeapData_u 	value;

} gnHeapNode_s;

typedef struct _gnHeap {

	long				index; /* last node / number of current nodes (complete-binary-tree array representation ...) */
	long				count; /* number of allocated nodes in ->pnode array */
	long				block; /* allocation block size expressed in number of nodes */
	gnHeapNode_s * 		pnode; /* the node-array */

} gnHeap_s;

extern void	gnHeapInit		(
							gnHeap_s * 		pheap
							);

extern void	gnHeapFree		(
							gnHeap_s * 		pheap
							);

extern int 	gnHeapInsertMax	(
							gnHeap_s * 		pheap ,
							long 			key ,
							gnHeapData_u 	value
							);

extern int	gnHeapExtractMax(
							gnHeap_s * 		pheap,
							gnHeapNode_s *	pnoderet
							);

extern int 	gnHeapInsertMin	(
							gnHeap_s * 		pheap ,
							long 			key ,
							gnHeapData_u 	value
							);

extern int	gnHeapExtractMin(
							gnHeap_s * 		pheap,
							gnHeapNode_s *	pnoderet
							);

__END_DECLS
#endif
