interface_build {
    {v.in.arc} 0
    {Преобразует данные из формата Arc/info в векторы GRASS,  и помещает результат в наборе данных пользователя.}
    {entry lines_in { Файл линий из ARC/INFO:} 0 arc}
    {entry points_in {Файл меток полигонов из ARC/INFO:} 0 arc}
    {entry text_in {Текстовый файл из ARC/INFO:} 0 arc}
    {entry vector_out {Векторная карта GRASS - результат:} 0 vector}
    {entry idcol  {Номер столбца текстового файла, содержащего ID.} 0 ""}
    {entry catcol {Номер столбца текстового файла, содержащего величины категорий.} 0 ""}
    {entry attcol {Номер столбца текстового файла, содержащего описание категорий.}  0 ""}
    {checkbox -n {Рамка.} "" -n}
    {checkbox type {Полигоны.} "" polygon}
    {checkbox type {Линии.} "" line}
}
