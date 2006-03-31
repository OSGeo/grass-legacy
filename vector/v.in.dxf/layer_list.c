#include <string.h>
#include "global.h"

void add_layer_to_list(char *layer)
{
    if (is_layer_in_list(layer))
	return;

    layers = (char **)G_realloc(layers, (num_layers + 2) * sizeof(char *));
    layers[num_layers++] = G_store(layer);
    layers[num_layers] = NULL;

    return;
}

int is_layer_in_list(char *layer)
{
    char **p;

    if (!layers)
	return 0;

    p = layers;
    while (*p && strcmp(layer, *p) != 0)
	p++;

    return *p != NULL;
}
