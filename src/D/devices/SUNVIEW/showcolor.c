#include <suntool/sunview.h>
#include <suntool/canvas.h>
#define CMS_SIZE        256
#define CAN_HEIGHT      10

main(argc, argv)
        char ** argv ;
{
        Frame frame ;
        Canvas canvas ;
        register Pixwin *pw;
        register int i ;
        unsigned char red[CMS_SIZE], green[CMS_SIZE], blue[CMS_SIZE] ;

        Pr_brush brush;
        brush.width = 3;

        frame = window_create(0, FRAME, FRAME_LABEL, argv[0],
            FRAME_ARGS, argc, argv, 0) ;
        canvas = window_create(frame, CANVAS, WIN_HEIGHT,
                               CAN_HEIGHT, WIN_WIDTH, 2 * CMS_SIZE, 0) ;
        window_fit(frame) ;
        pw = canvas_pixwin(canvas) ;

        for (i=0;i<CMS_SIZE;i++)
                red[i] = green[i] = blue[i] = i ;
        pw_setcmsname(pw, "showcolor") ;
        pw_putcolormap(pw, 0, CMS_SIZE, red, green, blue) ;

        for(i=0; i< CMS_SIZE; i++)
/*
                pw_vector(pw, i*2, 0, i*2, CAN_HEIGHT, 
                       PIX_SRC | PIX_COLOR(i), 1) ;
*/
                pw_rop(pw, i*2, 0, 2, CAN_HEIGHT,
                       PIX_SRC | PIX_COLOR(i), (Pixrect *)0, 0, 0) ;
        window_main_loop(frame) ;
        exit(0) ;
}
