/* this code is obsolete */
window_point (north, east)
    double north, east;
{
    diamond (window_col(east), window_row(north));
}

window_line (north1, east1, north2, east2)
    double north1, east1;
    double north2, east2;
{
    draw_line (window_col(east1), window_row(north1),
               window_col(east2), window_row(north2)
              );
}
