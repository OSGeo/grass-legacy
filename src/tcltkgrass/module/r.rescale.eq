interface_build {
    {r.rescale.eq} 0
    {Rescales the range of category values in a raster map layer to equalized histogram.}
    {entry input {Input raster map whose category values are to be rescaled:} 0 raster}
    {entry from {Category value range to be rescaled (min,max) [full range]:} 0 ""}
    {entry output {Output rescaled map:} 0 raster}
    {entry to {Rescaled category value range (min,max):} 0 ""}
    {entry title {Title of the output raster map ("title" in quotes):} 0 ""}
    {checkbox -z {Set map values outside specified range to 0 instead of null.} "" -z}
    {checkbox -q {Run quietly.} "" -q}
}
