conv_bits (bitfld)
    unsigned char *bitfld;
{
    int i;

    i = (bitfld[0] << 24) +
        (bitfld[1] << 16) +
        (bitfld[2] << 8) +
        (bitfld[3]);

    return (i);
}
