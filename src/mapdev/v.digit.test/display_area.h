/* display_area.c */
int reset_area(int, struct Map_info *);
int _reset_area (P_AREA *,struct Map_info *);
int erase_area(int, struct Map_info *);
int highlight_area(int, struct Map_info *);
int display_area(int, struct Map_info *);
int _display_area (P_AREA *,struct Map_info *);
int highlight_area_label(int,struct Map_info *);
int _highlight_area (P_AREA *,struct Map_info *);
int display_area_label(int, struct Map_info *);
int undisplay_area_label(int, struct Map_info *);
int unset_dot(double, double);
int reset_isle(int, struct Map_info *);
int erase_isle(int, struct Map_info *);
int highlight_isle(int, struct Map_info *);
int display_isle(int, struct Map_info *);
