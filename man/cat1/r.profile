


r.profile <main>     GRASS Reference Manual      <main> r.profile



NAME
     _r._p_r_o_f_i_l_e - Outputs the raster map layer values lying on
     user-defined line(s).
     (_G_R_A_S_S _R_a_s_t_e_r _P_r_o_g_r_a_m)

SYNOPSIS
     r.profile
     r.profile help
     r.profile map=_n_a_m_e [result=_t_y_p_e] [width=_v_a_l_u_e]
         line=_e_a_s_t,_n_o_r_t_h,_e_a_s_t,_n_o_r_t_h[,_e_a_s_t,_n_o_r_t_h,_e_a_s_t,_n_o_r_t_h,...]

DESCRIPTION
     This program outputs, in ASCII, the values assigned to those
     cells in a raster map layer that lie along one or more lines
     ("profiles").  The lines are described by their starting and
     ending coordinates.  The profiles may be single-cell wide
     lines, or multiple-cell wide lines.  The output, for each
     profile, may be the category values assigned to each of the
     cells, or a single aggregate value (e.g., average or median
     value).

     _r._p_r_o_f_i_l_e automatically orders the coordinates.  The user
     cannot specify the order for a specific profile; the
     direction is always northwest to southeast.

COMMAND LINE OPTIONS
     Parameters:

     map=_n_a_m_e          Raster map to be queried.

     result=_t_y_p_e       Type of result to be output.
                       Options:  raw, median, average
                       Default:  raw

                       Raw results output each of the category
                       values assigned to all cells along the
                       profile.  Median and average output a
                       single value per profile: average outputs
                       the average category value of all cells
                       under the profile;  median outputs the
                       median cell category value.

     line=_e_a_s_t,_n_o_r_t_h,_e_a_s_t,_n_o_r_t_h[,_e_a_s_t,_n_o_r_t_h,_e_a_s_t,_n_o_r_t_h,...]
                       The geographic coordinates of the starting
                       and ending points that define each profile
                       line, given as easting and northing
                       coordinate pairs.  The user must state the
                       starting and ending coordinates of at
                       least one line, and may optionally include
                       starting and ending coordinates of
                       additional lines.




GRASS 4.1                U.S. Army CERL                         1






r.profile <main>     GRASS Reference Manual      <main> r.profile



     width=_v_a_l_u_e       Profile width, in cells (odd number).
                       Default:  1

                       Wider profiles can be specified by setting
                       the width to 3, 5, 7, etc.  The profiles
                       are then formed as rectangles 3, 5, 7,
                       etc., cells wide.

OUTPUT FORMAT
     The output from this command is printed to the standard
     output in ASCII.  The format of the output varies slightly
     depending on the type of result.  The first number printed
     is the number of cells associated with the profile.  For raw
     output, this number is followed by the individual cell
     values.  For average and median output, this number is
     followed by a single value (i.e., the average or the median
     value).

     These examples are for the _e_l_e_v_a_t_i_o_n._d_e_m raster map layer in
     the _s_p_e_a_r_f_i_s_h sample data set distributed with GRASS 4.0:

     Single-cell profile:

          r.profile map=elevation.dem
          line=593655,4917280,593726,4917351

          4 1540 1551 1557 1550


     3-cell wide profile:
          r.profile map=elevation.dem
          line=593655,4917280,593726,4917351 width=3

          22 1556 1538 1525 1570 1555 1540 1528 1578 1565 1551
          1536 1523 1569 1557 1546 1533 1559 1550 1542 1552 1543
          1548

          (Output appears as multiple lines here, but is really
          one line)


     3-cell wide profile average:

          r.profile map=elevation.dem
          line=593655,4917280,593726,4917351 width=3
          result=average

          22 1548.363636


     3-cell wide profile median:




2                        U.S. Army CERL                 GRASS 4.1






r.profile <main>     GRASS Reference Manual      <main> r.profile



          r.profile map=elevation.dem
          line=593655,4917280,593726,4917351 width=3
          result=median

          22 1549.000000

SEE ALSO
     _d._p_r_o_f_i_l_e, _r._t_r_a_n_s_e_c_t

AUTHOR
     Michael Shapiro, U.S. Army Construction Engineering Research
     Laboratory











































GRASS 4.1                U.S. Army CERL                         3



