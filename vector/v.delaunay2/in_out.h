#ifndef IN_OUT_H
#define IN_OUT_H

int read_sites(int mode3d, int complete_map, struct Map_info map_in,
	       BOUND_BOX Box);
void output_edges(unsigned int n, int mode3d,
		  int Type, struct Map_info map_out);
void output_triangles(unsigned int n,
		      int mode3d, int Type, struct Map_info map_out);
void remove_duplicates(unsigned int *size);
int cmp(const void *a, const void *b);

#endif
