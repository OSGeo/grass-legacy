int getcoord(double *return_east, double *return_north);
double distance_from_pointer(double point_east, double point_north, char *location_east, char *location_north);

struct Postgres{
    char host[30];
    char def_table[30];
    char portnum[10];
    char database[30];
  };

/* void get_post_head(struct Postgres *postgres); */
