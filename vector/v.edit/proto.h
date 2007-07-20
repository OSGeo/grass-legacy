#ifndef _V_EDIT_PROTO
# define _V_EDIT_PROTO

/* args.c */
int parser(int argc, char*argv[], struct GParams *params, enum mode *action_mode);

/* delete.c */
int do_del(struct Map_info *Map, struct ilist *List, int print);

/* a2b.c */
int asc_to_bin(FILE *, struct Map_info *, struct ilist *);
int read_head ( FILE * dascii, struct Map_info *Map );
int do_close(struct Map_info *Map, int type, double thresh);
int do_snapping(struct Map_info *Map, struct ilist* List, int layer,
		double thresh, int to_vertex);

/* move.c */
int do_move(struct Map_info *Map, struct ilist *List, int print,
	    double move_x, double move_y);

/* vertex.c */
int do_move_vertex(struct Map_info *Map, struct ilist *List, int print,
		   struct Option *coord, double thresh,
		   double move_x, double move_y);
int do_remove_vertex(struct Map_info *Map, struct ilist *List, int print,
		     struct Option *coord, double thresh);
int do_add_vertex(struct Map_info *Map, struct ilist *List, int print,
		  struct Option* coord, double thresh);

/* break.c */
int do_break (struct Map_info *Map, struct ilist *List, int print,
	      int opt_npoints, double opt_x[], double opt_y[], double opt_z[], double thresh, struct ilist*);
int do_connect (struct Map_info *Map, struct ilist *List, int print,
		double thresh);

/* merge.c */
int do_merge(struct Map_info *Map, struct ilist *List, int print);

/* snap.c */
int do_snap(struct Map_info *Map, struct ilist *List, double thresh, int layer,
	    int print, struct ilist *List_updated);

int do_snap2(struct Map_info *Map, struct ilist *List, int layer,
	    int print, struct ilist *List_updated);

/* select.c */
int do_print_selected(struct ilist *List);
int merge_lists (struct ilist* alist, struct ilist* blist);
struct ilist* select_lines(struct Map_info *Map, enum mode action_mode,
			   struct GParams *params,
			   struct ilist *List);
int sel_by_cat(struct Map_info *Map, struct cat_list* cl_orig,
	       int layer, int type, char *cats,
	       struct ilist *List);
int sel_by_coordinates(struct Map_info *Map,
		       int layer, int type, struct Option *coords, double thresh,
		       struct ilist *List);
int sel_by_bbox(struct Map_info *Map,
		int layer, int type, struct Option *box,
		struct ilist *List);
int sel_by_polygon(struct Map_info *Map,
		   int layer, int type, struct Option *poly,
		   struct ilist *List);
int sel_by_id(struct Map_info *Map,
	      int layer, int type, char *ids,
	      struct ilist *List);
int sel_by_where(struct Map_info *Map,
		 int layer, int type, char *where,
		 struct ilist *List);
int reverse_selection(struct Map_info *Map, struct ilist **List);

/* max_distance.c */
double max_distance (double maxdistance);
double min_distance_line (struct line_pnts *Points1, struct line_pnts *Points2,
			  int *mindistidx);
void coord2bbox (double east, double north, double maxdist,
		 struct line_pnts *box);

/* cats.c */
int cats (struct Map_info *Map, struct ilist *List, int print,
	  int layer, int del, char *cats_list);

/* copy.c */
int do_copy (struct Map_info *Map, struct ilist *List, int print);

/* flip.c */
int do_flip (struct Map_info *Map, struct ilist *List, int print);

#endif
