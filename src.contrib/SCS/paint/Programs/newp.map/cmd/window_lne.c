/* this code is obsolete */
int 
window_point (double north, double east)
{
    diamond (window_col(east), window_row(north));
}

int 
window_line (double north1, double east1, double north2, double east2)
{
    draw_line (window_col(east1), window_row(north1),
               window_col(east2), window_row(north2)
              );
}
