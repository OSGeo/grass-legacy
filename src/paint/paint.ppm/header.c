header (rows, cols, max)
    int *rows, *cols, *max;
{
    char word[1024];

    if (!nextword(word)) return 0;
    if(word[0] != 'P' || word[1] != '6') return 0;

    if (!nextword(word) || sscanf (word, "%d", cols) != 1 || *cols < 1)
	return 0;
    if (!nextword(word) || sscanf (word, "%d", rows) != 1 || *rows < 1)
	return 0;
    if (!nextword(word) || sscanf (word, "%d", max) != 1 || *max < 1 || *max > 255)
	return 0;
    eol();
    return 1;
}
