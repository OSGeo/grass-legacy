
#define USE_PPM

#ifndef USE_PPM
void write_ycc();
#endif

void write_ppm();
void write_params();
void clean_files();
void parse_command();

char **gee_wildfiles();

int load_files();
int use_r_out();
