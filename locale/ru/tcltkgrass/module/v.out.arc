interface_build {
    {v.out.arc} 0
    {Преобразует векторный формат GRASS в формат ARC/INFO, и помещает результат в каталоге arc .}
    {entry vect {Векторный файл GRASS (input):} 0 vector}
    {entry arc_prefix {Префикс создаваемых файлов ARC/INFO:} 0 ""}
    {entry separator {Разделитель вывода [space]:} 0 ""}
    {checkbox type {Полигоны.} "" polygon}
    {checkbox type {Линии.} "" line}
}
