interface_build {
    {r.in.ll} 0
    {Converts raster data referenced using latitude and longitude coordinates to a UTM-referenced map layer.}
    {entry input {Input file:} 0 file}
    {entry output {Output raster map:} 0 raster}
    {entry bpc {Number of bytes per cell:} 0 ""}
    {entry corner {1 corner of the input ({nw|ne|sw|se},dd:mm:ss{N|S},ddd:mm:ss{E|W}):} 0 ""}
    {entry dimension {Dimension of the input (rows,columns):} 0 ""}
    {entry res {Resolution of the input, in arc second):} 0 ""}
    {entry spheroid {Spheroid to be used for coordinate conversion:} 0 spheroid}
    {checkbox -s {Signed data (high bit means negative value).} "" -s}
}
