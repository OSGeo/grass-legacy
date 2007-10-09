#ifndef _V_EDIT_PROTO
# define _V_EDIT_PROTO

/* args.c */
int parser(int, char* [], struct GParams *, enum mode *);

/* delete.c */
int do_delete(struct Map_info *, struct ilist *);

/* a2b.c */
int asc_to_bin(FILE *, struct Map_info *, struct ilist *);
int read_head ( FILE *, struct Map_info *);
int do_close(struct Map_info *, int, double);

/* move.c */
int do_move(struct Map_info *, struct ilist *,
	    double, double, int, double);

/* vertex.c */
int do_move_vertex(struct Map_info *, struct Map_info **, int,
		   struct ilist *,
		   struct line_pnts *, double ,
		   double, double,
		   int, int);
int do_remove_vertex(struct Map_info *, struct ilist *,
		     struct line_pnts *, double);
int do_add_vertex(struct Map_info *, struct ilist *,
		  struct line_pnts *, double);

/* break.c */
int do_break (struct Map_info *, struct ilist *,
	      struct line_pnts *, double,
	      struct ilist *);
int do_connect (struct Map_info *, struct ilist *,
		double);

/* merge.c */
int do_merge(struct Map_info *, struct ilist *);

/* snap.c */
int do_snap(struct Map_info *,
	    struct ilist *, double);
int do_snap_line(struct Map_info *,
	     int, int, double);
int do_snap_point(struct Map_info *,
		  int, double *, double *, double *, double,
		  int);
int do_snapping(struct Map_info *, struct Map_info **, int,
		struct ilist*,
		double, int);

/* select.c */
int do_print_selected(struct ilist *);
int merge_lists (struct ilist*, struct ilist*);
struct ilist* select_lines(struct Map_info *, enum mode,
			   struct GParams *,
			   struct ilist *);
int sel_by_cat(struct Map_info *, struct cat_list*,
	       int, int, char *,
	       struct ilist *);
int sel_by_coordinates(struct Map_info *,
		       int, struct Option *, double,
		       struct ilist *);
int sel_by_bbox(struct Map_info *,
		int, struct Option *,
		struct ilist *);
int sel_by_polygon(struct Map_info *,
		   int, struct Option *,
		   struct ilist *);
int sel_by_id(struct Map_info *,
	      char *,
	      struct ilist *);
int sel_by_where(struct Map_info *,
		 int, int, char *,
		 struct ilist *);
int reverse_selection(struct Map_info *, struct ilist **);

/* max_distance.c */
double max_distance (double);
double min_distance_line (struct line_pnts *, struct line_pnts *,
			  int *);
void coord2bbox (double, double, double,
		 struct line_pnts *);

/* cats.c */
int cats (struct Map_info *, struct ilist *,
	  int, int, char *);

/* copy.c */
int do_copy (struct Map_info *, struct Map_info *, struct ilist *);

/* flip.c */
int do_flip (struct Map_info *, struct ilist *);

#endif
