#include "usgs.h"
getgrid()
{
        int roww,br_while,done;
        int last, found, found_in_this_prof;

        found = 0;  /* flag that indicates if some correct data was found*/
/*  read first profile from file          */

        get_profile();

        col = w;
/*  start reading tape, skipping over unneeded records */

/* skip to the east edge of data */

    while(col - x_res/2. > P_col)
        {

        /*  continue reading remainder of last profile */

                for(roww = 0;roww < rows;roww++)
                        buffer += get_int(&elev);

        /*  now ready to get next profile */

                if(bas_e >= P_cols) break;
		get_profile();
        }

/*  loop thru current region until west edge of data reached */

        skip_col = 0;
        br_while = 0;
        if(skip_columns())
                while (col <= e)
                {
                        skip_row = 0;r =0;
                        row = s; cur_row = P_row;
                        G_zero (profile_buf,profile_buf_size);

                /* find south edge of data: */

		        found_in_this_prof = 1;
			roww = 1;
                        while(row - y_res/2. > cur_row)
                        {

                              /*  if ((row - y_res/2.) >= (P_row + (rows - 1) * y_res)) this is supposedly the same thing */
				if(roww==rows)
				/* we hit the end of the profile */
				{
				  found_in_this_prof = 0;
				  break;
				}
			        /*return 0*/
                                /* this is wrong since thee might still be other profiles in this file which contain the data */
				else
				{
                                  buffer += get_int(&elev);
                                  cur_row += y_res;
				  roww++;
				}
                        }

                        if(found_in_this_prof==0) 
			{
			  int next;

                          col += cellhd.ew_res;
                          skip_col++;
			  next = next_profile();
			  if(next<=0)
			  /* no more profiles */
			      br_while = 1;
			 /* break main loop */
                        }
			else 
			     found = 1;

			if(br_while)break;
			if(!found_in_this_prof) continue;
			/*****************************************/
                        buffer +=  get_int(&elev);
                        elev = ((elev * z_res) + bas_elev);

                /* loop thru and extract elevation values according to
                   resolution of cell header file                      */

                        if(skip_rows())
                                while (row <= n)
                                {
                                        profile_buf[r++] = elev;
                                        row += cellhd.ns_res;
                                        if(!next_elev())
                                                break;
                                }

                /* read rest of elevations in profile */

                        while(cur_row < P_row + (rows - 1)*y_res)
                        {
                                buffer += get_int(&elev);
                                cur_row += y_res;
                        }

                /* write profile to temporary file */

                        done = 0;
                        while(!done)
                        {
                                long offset;
                                offset = (skip_col*cellhd.rows+skip_row) * sizeof (CELL);
                                lseek(fd,offset,0);
                                if(write(fd,profile_buf,r*sizeof(CELL)) != r*sizeof(CELL))
                                {
					unlink(inf);
					unlink(of);
                                        G_fatal_error("error while writing to temp file");
                                }
                                col += cellhd.ew_res;
                                skip_col++;
                                switch(next_profile())
                                {
                                        case -1:
                                                br_while = 1;
                                                done = 1;
                                                break;
                                        case 0 :
                                                break;
                                        default:
                                                done = 1;
                                                break;
                                }
                        }
                        if(br_while)/*||(P_cols == bas_e)*/
                           break;
                }

    if(!found) return(0);
/*  read rest of profiles in the file */
    if(P_cols==bas_e) last = 1;
    /* that means we don't need to read anymore elevatons */
    else last = 0;

    while(bas_e < P_cols)
        /*  continue reading remainder of profiles */
        {

        /*  continue reading remainder of the last profile */

          for(roww = 0;roww < rows;roww++)
                        buffer += get_int(&elev);

        /*  now ready to get next profile */
		get_profile();
        }
	/* now bas_e == P_cols */
	if(!last)
          for(roww = 0;roww < rows;roww++)
                        buffer += get_int(&elev);
	/*  need to read type C record: */
	if(C_record)
	  {
             next_record();
	     record_pos = record_pos + 1024;
	  }
        return(1);
}



skip_columns()
{
        while((col + x_res/2.) < P_col)
        {
                if((col += cellhd.ew_res) > e)
                        return(0);
                skip_col++;
        }
        return(1);
}

skip_rows()
{
        while((row + y_res/2.) < P_row)
        {
                if((row += cellhd.ns_res) > n)
                        return(0);
                skip_row++;
        }
        return(1);
}

next_profile()
{
        int profiles_got;
        int rw ;

        profiles_got = 0;
        while(col - x_res/2. > P_col)
        {
           if(profiles_got >0)
                for(rw = 0;rw < rows;rw++)
           buffer += get_int(&elev);
           if(bas_e >= P_cols)
               return -1;
           get_profile();
           profiles_got++;
	}

	/* prof_processed = prof_processed + profiles_got; */
        return(profiles_got);
}

next_elev()
{
    while(row - y_res/2. > cur_row)
        {
                if ((row - y_res/2.) > (P_row + (rows - 1) * y_res))
                        return(0);
                buffer += get_int(&elev);
                cur_row += y_res;
                elev = ((elev * z_res) + bas_elev);
        }
        return(1);
}
