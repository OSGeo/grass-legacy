
#ifdef SWITCHER
#define GLOBAL
#else
#define GLOBAL extern
#endif

#define FIFO

GLOBAL int cur_x;
GLOBAL int cur_y;

GLOBAL double _text_size_x;
GLOBAL double _text_size_y;
GLOBAL double _text_rotation;
