#ifdef FOO
#include "Vect.h"
#endif

#define A_out 0.35
#define A_in  0.10
#define B_out 0.25
#define B_in  0.35

#ifdef FOO
int corridor (
    struct Map_info *Cmap,struct Map_info *Tmap,
    plus_t line,
    struct line_pnts *Points,
/*DEBUG*/     FILE *fpp)
#endif
{
    static struct line_pnts APoints, BPoints;
    static int first_time = 1;
    double dx, dy;
    plus_t A_list[50], B_list[50];
    int A_cnt, B_cnt;
    double A_N, A_S, A_E, A_W;
    double B_N, B_S, B_E, B_W;
    int i;

    if (first_time)
    {
	APoints.alloc_points = BPoints.alloc_points = 0;
	if (0 > dig_alloc_points (&APoints, 5))
	    return (-1);
	if (0 > dig_alloc_points (&BPoints, 5))
	    return (-1);
	first_time = 0;
    }

    dx = Points.x[3] - Points.x[4];
    dy = Points.y[3] - Points.y[4];

    APoints.x[0] = APoints.x[4] = Points.x[0] - dx * A_out;
    APoints.y[0] = APoints.y[4] = Points.y[0] - dy * A_out;
    APoints.x[1] = Points.x[1] - dx * A_out;
    APoints.y[1] = Points.y[1] - dy * A_out;
    APoints.x[2] = Points.x[1] + dx * A_in;
    APoints.y[2] = Points.y[1] + dy * A_in;
    APoints.x[3] = Points.x[0] + dx * A_in;
    APoints.y[3] = Points.y[0] + dy * A_in;
    APoints.n_points = 5;

    BPoints.x[0] = BPoints.x[4] = Points.x[3] - dx * B_in;
    BPoints.y[0] = BPoints.y[4] = Points.y[3] - dy * B_in;
    BPoints.x[1] = Points.x[2] - dx * B_in;
    BPoints.y[1] = Points.y[2] - dy * B_in;
    BPoints.x[2] = Points.x[2] + dx * B_out;
    BPoints.y[2] = Points.y[2] + dy * B_out;
    BPoints.x[3] = Points.x[3] + dx * B_out;
    BPoints.y[3] = Points.y[3] + dy * B_out;
    BPoints.n_points = 5;

    dig_bound_box2 (&APoints, &A_N, &A_S, &A_E, &A_W, Cmap->head.orig_scale); 
    /*4.0*/
    dig_bound_box2 (&BPoints, &B_N, &B_S, &B_E, &B_W, Cmap->head.orig_scale);      /*4.0*/

    A_cnt = B_cnt = 0;

    for (Cnode = 1 ; Cnode <= Cmap->n_nodes ; Cnode++)
    {
	CNode = &(Cmap->Node[Cnode]);
	if (CNode->y < A_S || CNode->y > A_N) goto part_B;
	if (CNode->x < A_W || CNode->x > A_E) goto part_B;

	if (0.0 == dig_point_in_poly (CNode->x, CNode->y, &APoints))
	    goto part_B;
	
	A_list[A_cnt++] = Cnode;
part_B:
	if (CNode->y < B_S || CNode->y > B_N) continue;
	if (CNode->x < B_W || CNode->x > B_E) continue;

	if (0.0 == dig_point_in_poly (CNode->x, CNode->y, &BPoints))
	    continue;
	
	B_list[B_cnt++] = Cnode;
    }

    for (i = 0 ; i < A_cnt ; i++)
    {
	dist1 = dig_distance2_point_to_line (Cmap->Node[A_list[i]].x,
	   Cmap->Node[A_list[i]].y, Points.x[0], Points.y[0],
	   Points.x[1], Points.y[1]);

	if (!A_Best)
	{
	    A_Best = A_list[i];
	    A_Dist = dist1;
	}
	else
	{
	    if (dist1 < A_Dist)
	    {
		A_Best = A_list[i];
		A_Dist = dist1;
	    }
	}
    }
    for (i = 0 ; i < B_cnt ; i++)
    {
	dist1 = dig_distance2_point_to_line (Cmap->Node[B_list[i]].x,
	   Cmap->Node[B_list[i]].y, Points.x[2], Points.y[2],
	   Points.x[3], Points.y[3]);

	if (!B_Best)
	{
	    B_Best = B_list[i];
	    B_Dist = dist1;
	}
	else
	{
	    if (dist1 < B_Dist)
	    {
		B_Best = B_list[i];
		B_Dist = dist1;
	    }
	}
    }
    if (A_Best) 
    {
	A = A_Best;
	num_found ++;
    }
    if (B_Best) 
    {
	B = B_Best;
	num_found ++;
    }
}
