extern int cur_x, cur_y;
double _text_size_x, _text_size_y, _text_rotation;

Text(text)
char *text;
{
    extern int soft_text();

    soft_text(cur_x, cur_y, _text_size_x, _text_size_y, _text_rotation,
			text);
}
