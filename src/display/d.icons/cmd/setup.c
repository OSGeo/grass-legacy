
setup_plot()
{
    int D_move_abs(), D_cont_abs();
    double D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east();

    D_setup(0);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);
}
