#include "usgs.h"
getgrid()
{
        int roww,br_while,done;
        int y,x;

/*  read first profile from file          */

        get_profile();

        col = w;
/*  start reading tape, skipping over unneeded records */

    while(col - x_res/2. > P_col)
        {

        /*  continue reading remainder of last profile */

                for(roww = 0;roww < rows;roww++)
                        buffer += get_int(&elev);

        /*  now ready to get next profile */

                if(!(get_profile()))
                        return(0);
        }


/*  loop thru current window until west edge of data reached */

        skip_col = 0;
        br_while = 0;
        if(skip_columns())
                while (col <= e)
                {
                        skip_row = 0;r =0;
                        row = s; cur_row = P_row;
                        G_zero (profile_buf,profile_buf_size);

                /* find south edge of data */

                        while(row - y_res/2. > cur_row)
                        {

                                if ((row - y_res/2.) > (P_row + (rows - 1) * y_res))
                                        return(0);
                                buffer += get_int(&elev);
                                cur_row += y_res;
                        }

                        buffer +=  get_int(&elev);
                        elev = ((elev * z_res) + bas_elev);

                /* loop thru and extract elevation values according to
                   resolution of cell header file                      */

                if(bas_e == 3) {
                        if(skip_rows())
                                while (row <= n)
                                {
                                        profile_buf[r++] = elev;
                                        row += cellhd.ns_res;
                                        if(!next_elev())
                                                break;
                                }
                        }
                else
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
                                        G_fatal_error("error while writing to cell file");
                                        exit(1);
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
                        if((br_while)||(P_cols == bas_n))
                           break;
                }

/*  read rest of file */

        if(filestat)
                skip_file();
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
        if(profiles_got != 0)
                for(rw = 0;rw < rows;rw++)
                        buffer += get_int(&elev);
                if(!(get_profile()))
                        return(-1);
                profiles_got++;
        }
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
