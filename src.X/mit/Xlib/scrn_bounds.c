#include <X11/Xlib.h>


XD_get_screen_bounds(xl, xr, yt, yb, n, s, w, e, width, height)
double *xl, *xr, *yt, *yb, n, s, w, e;
int width, height;
{
    double w_width, w_height, w_ratio, s_ratio, w_req, h_req;

    w_width = e - w;
    w_height = n - s;
    w_ratio = w_width / w_height;

    s_ratio = (double) width / (double) height;

    if (s_ratio >= w_ratio) {
        *yt = 0.0;
        *yb = height;
        w_req = height * w_ratio;
        *xl = (width - w_req) / 2.0;
        *xr = *xl + w_req;
    } else {
        *xl = 0.0;
        *xr = width;
        h_req = width / w_ratio;
        *yt = (height - h_req) / 2.0;
        *yb = *yt + h_req;
    }





}
