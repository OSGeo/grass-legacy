interface_build {
    m.ipf 0
    {Iterative proportional fitting for error matrices}
    {entry input {Input ASCII file of error matrix:} 0 file}
    {entry format {Format conversion string to print results (see man page for printf) [default=%7.3f]:} 0 ""}
    {entry stop {Specify number indicating max. iteration and min. fractional change in marginals [default=100.01=100 iterations & 0.01 change]:} 0 ""}
    {checkbox -e {Indicate when iterative algorithm is finished.} "" -e}
    {checkbox -m {Print marginal values (row & column totals) with each matrix.} "" -m}
    {checkbox -z {Print intermediate (smoothed) matrix.} "" -z}
}
