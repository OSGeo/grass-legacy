

struct box *update_landpatt(ptr)
	struct box *ptr;

{

	extern char path[], buf[];
	extern struct box *land_patt_ptr;
	extern int layer_count,  land_patt_count;


	G__file_name(path, "landpatterns", "", G_mapset());
	free_child_ring(land_patt_ptr);
	 sprintf(buf,"ls %s",path);
	read_from_dir(land_patt_ptr);	
	land_patt_count = layer_count;

	return(ptr);
}



struct box *update_user_cell(ptr)
        struct box *ptr;
{
        extern struct box *user_cell_ptr;
        extern char buf[];
        extern int layer_count,  user_cell_count;
        extern char path[];
         
        G__file_name(path,"cell","",G_mapset());
        free_child_ring(user_cell_ptr);
        sprintf(buf,"ls %s",path);
        read_from_dir(user_cell_ptr);
        user_cell_count = layer_count;

        return(ptr);
}


	
