/* "N_dr_me_pr_e" function will draw, measure, print, or extract the line    */
/* you have measured.  The value in variable "DSLE_flag" will determine      */
/* which operation that this function will perform.                          */
/* If DPE_flag= 'D' then this function will draw and measure your measured   */
/* line.                                                                     */
/* If DPE_flag= 'P' then this function will draw, measure, and print a       */
/* listing of the measured lines going from node to node.                    */
/* If DPE_flag= 'E' then this function will draw, measure, and extract and   */
/* create an ascii digit file of the line measured.                          */

#include "distance.h"
int
N_dr_me_pr_e(map,p,DPE_flag)
 struct Map_info *map;
 struct line_pnts *p;
 char DPE_flag;
 {
  int i;
  extern int draw_seg();
  int prev_num, node_num;
  struct NODE_TABLE *ptr_n_begin, *ptr_n_end;
  struct NODE_TABLE *prev_ptr;
  double dist;
  int first_node_num, last_node_num;

#ifdef DEBUG
fprintf(stderr,"N_dr_me_pr_e\n");
#endif DEBUG
  if (node_t_info.count < 2)
    return(0);
/* Set cummulative and direct distances to 0 */
  node_t_info.cum_dist = 0.0;
  node_t_info.dir_dist = 0.0;
  first_node_num = ptr_node->node_number;
  ptr_node_var = ptr_node + (node_t_info.count-1);
  last_node_num = ptr_node_var->node_number;
  ptr_n_begin = ptr_node + 1;
  ptr_n_end   = ptr_node + node_t_info.count;
  for (ptr_node_var=ptr_n_begin; ptr_node_var < ptr_n_end; ptr_node_var++)
   {
    prev_ptr = ptr_node_var - 1;
    prev_num = prev_ptr->node_number;
    node_num = ptr_node_var->node_number;
/* Draw line in "red" */
    draw_seg(map,p,map->Node[prev_num].y,map->Node[prev_num].x,map->Node[node_num].y,map->Node[node_num].x,MA_COLOR);
/* Draw node plusses for previous and present nodes */
    draw_NODE(map,p,prev_num,IN_COLOR);
    draw_NODE(map,p,node_num,IN_COLOR);
/* Calculate distance of "red" partial segment */
    dist_seg_N(map,p,map->Node[prev_num].y,map->Node[prev_num].x,map->Node[node_num].y,map->Node[node_num].x,prev_num,node_num,DPE_flag,&dist);
    node_t_info.cum_dist += dist;
   }
/* Calculate value for "node_t_info.dir_dist" */
  distance(map->Node[first_node_num].x,map->Node[first_node_num].y,map->Node[last_node_num].x,map->Node[last_node_num].y,&dist);
  node_t_info.dir_dist = dist;
  return(1);
 }
