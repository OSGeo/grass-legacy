frame .main_menu
pack .main_menu -expand yes -fill both

# menus used several times in the main menu

set monitors_menu {
    "Старт" "" {
        "Все активные X" "" {start_monitors}
        -separator
        X0 "" {"start_monitor x0"}
        X1 "" {"start_monitor x1"}
        X2 "" {"start_monitor x2"}
        X3 "" {"start_monitor x3"}
        X4 "" {"start_monitor x4"}
        X5 "" {"start_monitor x5"}
        X6 "" {"start_monitor x6"}
        -separator
        CELL "" {"exec xterm -iconic -e d.mon start=CELL"}
    }
    "Стоп" "" {
        "Все X" "" {stop_monitors}
        -separator
        X0 "" {"stop_monitor x0"}
        X1 "" {"stop_monitor x1"}
        X2 "" {"stop_monitor x2"}
        X3 "" {"stop_monitor x3"}
        X4 "" {"stop_monitor x4"}
        X5 "" {"stop_monitor x5"}
        X6 "" {"stop_monitor x6"}
        -separator
        CELL "" {"exec xterm -iconic -e d.mon stop=CELL"}
    }
    "Выбрать" "" {
        X0 "" {"exec xterm -iconic -e d.mon select=x0"}
        X1 "" {"exec xterm -iconic -e d.mon select=x1"}
        X2 "" {"exec xterm -iconic -e d.mon select=x2"}
        X3 "" {"exec xterm -iconic -e d.mon select=x3"}
        X4 "" {"exec xterm -iconic -e d.mon select=x4"}
        X5 "" {"exec xterm -iconic -e d.mon select=x5"}
        X6 "" {"exec xterm -iconic -e d.mon select=x6"}
        -separator
        CELL "" {"exec xterm -iconic -e d.mon select=CELL"}
    }
    "Параметры X-дисплея" "" {
        "source $env(TCLTKGRASSBASE)/module/d.mon"
    }
}

set display_raster {
    "Показать растровую карту" "" {
        "source $env(TCLTKGRASSBASE)/module/d.rast"
    }
    "Показать величины HIS" "" {
        "source $env(TCLTKGRASSBASE)/module/d.his"
    }
    "Наложение RGB" "" {
        "source $env(TCLTKGRASSBASE)/module/d.rgb"
    }
    "3D-изображение" "" {
        "source $env(TCLTKGRASSBASE)/module/d.3d"
    }
    "Теневой растр" "" {
        "source $env(TCLTKGRASSBASE)/module/d.shadedmap"
    }
    "Профиль" "" {
        "source $env(TCLTKGRASSBASE)/module/d.profile"
    }
}

set display_vector {
    "Показать векторную карту" "" {
        "source $env(TCLTKGRASSBASE)/module/d.vect"
    }
    "Показать площади с метками" "" {
        "source $env(TCLTKGRASSBASE)/module/d.vect.area"
    }
    "Показать линии с метками" "" {
    	"source $env(TCLTKGRASSBASE)/module/d.vect.line"
    }
}
set display_sites {
    "Показать объекты" "" {
        "source $env(TCLTKGRASSBASE)/module/d.sites"
    }
    "Показать названия объектов" "" {
        "source $env(TCLTKGRASSBASE)/module/d.site.labels"
    }
    -separator
    "Показать точки" "" {
        "source $env(TCLTKGRASSBASE)/module/d.points"
    }
    "Показать точки (икон.)" "" {
        "source $env(TCLTKGRASSBASE)/module/d.icons"
    }
}

set image_processing {
    "Создать/Редактировать группу" "" {
        "run i.group &"
    }
    "Выбор цели для группы" "" {
        "run i.target &"
    }
    -separator
    Ректификация "" {
         "Выбор контрольных точек (GCP)" "" {
             "run i.points &"
         }
         "Афинное и Полиномное преобразование" "" {
             "run i.rectify &"
         }
         
         "Орто-фоторектификация" "" {
             "run i.ortho.photo &"
         }
    }
    "Фильтры" "" {
         "Нахождение границ ZECD" "" {
             "source $env(TCLTKGRASSBASE)/module/i.zc"
         }
         "Фильтрация заданной матрицей" "" {
             "source $env(TCLTKGRASSBASE)/module/r.mfilter"
         }
	 "Оттенки серого вдоль по гистограмме" "" {
	     "source $env(TCLTKGRASSBASE)/module/i.grey.scale"
	 }
    }
    "Преобразования" "" {
         "Канонические компоненты" "" {
             "source $env(TCLTKGRASSBASE)/module/i.cca"
         }
         "Главные компоненты" "" {
             "source $env(TCLTKGRASSBASE)/module/i.pca"
         }
         "Быстрое преобразование Фурье (FFT)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.fft"
         }
         "Задать/Удалить маску для FFT" "" {
             "run r.mask &"
         }
         "Обратное быстрое преобразование Фурье" "" {
             "source $env(TCLTKGRASSBASE)/module/i.ifft"
         }
    }
    Классификация "" {
         "Кластерный ввод для автоматической классификации" "" {
             "source $env(TCLTKGRASSBASE)/module/i.cluster"
         }
         "Интерактивный ввод для пользовательской классификации" "" {
             "run i.class &"
         }
         "Неинтерактивный ввод для пользовательской классификации(MLC)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.gensig"
         }
         "Неинтерактивный ввод для пользовательской классификации(SMAP)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.gensigset"
         }
         -separator
         "Классификация максимального правдоподобия(MLC)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.maxlik"
         }
         "Классификация последовательного максимума (SMAP)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.smap"
         }
    }
}

set misc {
    "Преобразования координат" "" {
	"Проекция/Координаты" "" {
	    "run m.proj &"
	}
	"Коррекция сфероида" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.datum.shift"
	}
	"геоцентр.- шир/дол" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.gc2ll"
	}
	"шир/дол - геоцентр." "" {
	    "source $env(TCLTKGRASSBASE)/module/m.ll2gc"
	}
	"UTM - шир/дол" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.u2ll"
	}
	"шир/дол - UTM" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.ll2u"
	}
    }
    -separator
    "DEM/DTED" "" {
	"Изображение DEM" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dem.examine"
	}
	"Получение DEM" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dem.extract"
	}
	"Изображение DTED" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dted.examine"
	}
	"Получение DTED" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dted.extract"
	}
    }
    -separator
    "Другие" "" {
	"Поворот данных высот на 90 град." "" {
	    "source $env(TCLTKGRASSBASE)/module/m.rot90"
	}
	"Зеркальное отображение высот" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.flip"
	}
	"CTG data  из USGS lulc-файла" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.lulc.USGS"
	}
	"Информация по Tiger Region" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.tiger.region"
	}
	"UTM регион - lat/lon регион" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.region.ll"
	}
    }
}

# main menu

menu_build 1 .main_menu {
    Парам "Параметры TCLTKGRASS" {
        Монитор "" $monitors_menu
        "Окна модулей" "" {
            "Задать размер автоматически" "" {
                "resize_menu; resize $module_list"
            }
            "Заново открыть все активные окна" "" {
                "resize_menu; reinit_modules"
            }
            -separator
            "Закрыть все окна модулей" "" {
                unmap_modules
            }
            "Восстановить все активные окна" "" {
                map_modules
            }
            -separator
            "Сократить все активные окна" "" {
                "foreach module $module_list {catch {wm iconify .$module; \
                                                     wm iconify .$module.print}}"
            }
            "Раскрыть все активные окна" "" {
                "foreach module $module_list {catch {wm deiconify .$module; \
                                                     wm deiconify .$module.print}}"
            }
            -separator
            "Закрыть все активные окна" "" {
                "foreach module $module_list {catch {destroy .$module}}"
            }
        }
        "Размер меню автоматически" "" {resize_menu}
        -separator
	Скрипты "" {
	    "Редактировать скрипт" "" {
		"script_start"
	    }
	    "Остановить скрипт"  "" {
		"script_stop"
	    }
	    "Запуск скрипта"     "" {
		"script_play"
	    }
	}

	-separator
        Опции "" {
	"Фонт меню" "" {
                "fontsel {Menu font} main_menu(font);\
                 setfont .main_menu $main_menu(font);\
                 resize_menu"
            }
            "Фонт модулей" "" {
                "fontsel {Module font} module_font;\
                 foreach module $module_list {setfont .$module $module_font} ;\
                 resize $module_list"
            }
            "Фонт результата" "" {
                "fontsel {Result font} result_font"
            }
            "Фонт диалога" "" {
                "fontsel {Dialog font} dialog_font"
            }
            -separator
            "Размер дисплея" "" {
                setdisplay
            }
	    -separator
	    "Конфигурация браузера" "" {
		"config_netscape"
	    }
        }
        -separator
        "Сохранить параметры" "" {
            "tcltkgrass_save ."
        }
    }
    Карты "Параметры и операции с картами" {
        "Список" "" {
            "source $env(TCLTKGRASSBASE)/module/g.list"
        }
        "Копировать" "" {
            "source $env(TCLTKGRASSBASE)/module/g.copy"
        }
        "Переименовать" "" {
            "source $env(TCLTKGRASSBASE)/module/g.rename"
        }
        "Удалить" "" {
            "source $env(TCLTKGRASSBASE)/module/g.remove"
        }
        -separator
        "Наборы данных" "" {
            "run g.access &"
        }
        "Поменять набор данных" "" {
            "source $env(TCLTKGRASSBASE)/module/g.mapsets"
        }
        "Удалить набор данных" "" {
            "source $env(TCLTKGRASSBASE)/module/mapset.remove"
        }
        -separator
        "Создать/Редактировать группу" "" {
            "run i.group &"
        }
        "Выбор цели для группы" "" {
            "run i.target &"
        }
    }
    Регион "Границы региона" {
    	
        "Увеличить/уменьшить на экране" "" {
            "source $env(TCLTKGRASSBASE)/module/d.zoom"
        }
        "Переместить на экране" "" {
            "source $env(TCLTKGRASSBASE)/module/d.pan"
        }
        "Установить/Удалить маску" "" {
            "run r.mask &"
        }
        -separator
	"Масштаб карты на экране" "" {
            "source $env(TCLTKGRASSBASE)/main/monscale.tcl"
        }
        "Показать параметры региона" "" {
            "run g.region -p &"
        }
        "Задать по умолчанию" "" {
            "exec g.region -d; exec d.erase"
        }
        "Изменить параметры" "" {
            "source $env(TCLTKGRASSBASE)/module/g.region.sh"
        }
    }
    Дисплей "Показать карты" {
       Монитор "" $monitors_menu
       "Масштаб карты на экране" "" {
            "source $env(TCLTKGRASSBASE)/main/monscale.tcl"
        }
        -separator
        Растр "" $display_raster
        Векторы "" $display_vector
        Объекты "" $display_sites
        Текст "" {
            "Показать название карты" "" {
                "source $env(TCLTKGRASSBASE)/module/d.title.sh"
            }
            "Показать легенду" "" {
                "source $env(TCLTKGRASSBASE)/module/d.legend"
            }
            "Показать текстовые метки" "" {
                "source $env(TCLTKGRASSBASE)/module/d.label"
            }
            "Показать текстовые метки для paint" "" {
                "source $env(TCLTKGRASSBASE)/module/d.paint.labels"
            }
            "Выбрать фонт" "" {
                "source $env(TCLTKGRASSBASE)/module/d.font"
            }
            "Показать текст" "" {
                "source $env(TCLTKGRASSBASE)/module/d.text"
            }
        }
        Графика "" {
            "Показать цветовую таблицу" "" {
                "source $env(TCLTKGRASSBASE)/module/d.colortable"
            }
            "Показать геолинии" "" {
                "source $env(TCLTKGRASSBASE)/module/d.geodesic"
            }
            "Показать румбы" "" {
                "source $env(TCLTKGRASSBASE)/module/d.rhumbline"
            }
            "Масштабная линейка" "" {
                "source $env(TCLTKGRASSBASE)/module/d.barscale"
            }
            "Наложить сетку" "" {
                "source $env(TCLTKGRASSBASE)/module/d.grid"
            }
            "Показать гистограмму" "" {
                "source $env(TCLTKGRASSBASE)/module/d.histogram"
            }
            "Показать легенду" "" {
                "source $env(TCLTKGRASSBASE)/module/d.legend"
            }
        }
        "Местонахождение" "" {
            "source $env(TCLTKGRASSBASE)/module/d.where"
        }
	-separator
        "3D визуализация NVIZ" "" {
            "source $env(TCLTKGRASSBASE)/module/nviz"
        }
	"Очистить дисплей" "" {
            "source $env(TCLTKGRASSBASE)/module/d.erase"
        }
        "Увеличить/уменьшить на экране" "" {
        "source $env(TCLTKGRASSBASE)/module/d.zoom"
        }
        "Параметры рамки дисплея" "" {
            "source $env(TCLTKGRASSBASE)/module/d.frame"
        }
        "Тип цветовой таблицы" "" {
            "source $env(TCLTKGRASSBASE)/module/d.colormode"
        }
    }
    Растр "Растровый анализ" {
        Показать "" $display_raster
        "Анализ" "" {
            "Запрос мышью" "" {
                "source $env(TCLTKGRASSBASE)/module/d.what.rast"
            }
            "Запрос через файл" "" {
                "source $env(TCLTKGRASSBASE)/module/r.what"
            }
            "Профиль" "" {
                "source $env(TCLTKGRASSBASE)/module/d.profile"
            }
	    "Измерить длину и площадь" "" {
                "source $env(TCLTKGRASSBASE)/module/d.measure"
            }
            "Оверлей" "" {
                "Задать маску" "" {
                    "run r.mask &"
                }
                "Кросс" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.cross"
                }
                "Заплатка" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.patch"
                }
                "Оверлей с выбором по критерию" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.infer"
                }
                "Экспертная система Байеса" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.binfer"
                }
                "Растровые вычисления" "" {
                    "source $env(TCLTKGRASSBASE)/module/mapcalculator"
                }
                "Логические операции" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.combine"
                }
                "Присвоение весов" "" {
                    "run r.weight &"
                }
            }
            "Арифметическое сглаживание" "" {
                "Анализ по соседним точкам" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.neighbors"
                }
                "Буфферы" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.buffer"
                }
                "Расширение границ" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.grow"
                }
                "Утоньшение растровых линий" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.thin"
                }
            }
            "Территория" "" {
                "Создание карт водоразделов" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.basins.fill"
                }
                "Моделирование водоразделов" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.watershed"
                }
                "Стоимость пути между двумя точками" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.cost"
                }
                "Нахождение траектории потока" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.drain"
                }
                "Вычисление карты линий тока" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.flow"
                }
                "Склон и экспозиция" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.slope.aspect"
                }
                "Угол зрения" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.los"
                }
                "Создание теневого растра" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.shadedmap"
                }
            }
        }
	"Преобразовать растр" "" {
            "в объекты" "" {
                "source $env(TCLTKGRASSBASE)/module/r.to.sites"
            }
        }
        "Преобразовать" "" {
            "в линии из тонкого растра" "" {
                "source $env(TCLTKGRASSBASE)/module/r.line"
            }
            "в полигоны" "" {
                "source $env(TCLTKGRASSBASE)/module/r.poly"
            }
            "в контурные линии" "" {
                "source $env(TCLTKGRASSBASE)/module/r.contour"
            }
        }
        "Операции" "" {
            "Создать/Редактировать файл описания" "" {
                "run r.support &"
            }
            "Реклассификация категорий" "" {
                "source $env(TCLTKGRASSBASE)/module/r.reclass"
            }
            "Изменить масштаб категорий" "" {
                "source $env(TCLTKGRASSBASE)/module/r.rescale"
            }
            "Усреднение" "" {
                "source $env(TCLTKGRASSBASE)/module/r.average"
            }
            "Статистические вычисления" "" {
                "source $env(TCLTKGRASSBASE)/module/r.statistics"
            }
            "Укрупнить малые площади" "" {
                "source $env(TCLTKGRASSBASE)/module/r.clump"
            }
            -separator
            "Создать цветовую таблицу" "" {
                "source $env(TCLTKGRASSBASE)/module/r.colors"
            }
            "Редактировать цветовую таблицу" "" {
                "source $env(TCLTKGRASSBASE)/module/d.colors"
            }
	    "Цв. таблица по контраст. гистограмме оттенков серого" "" {
		"source $env(TCLTKGRASSBASE)/module/i.grey.scale"
	    }
            -separator
            "Дигитайзер" "" {
                "run r.digit &"
            }
            "Растровые вычисления" "" {
                "source $env(TCLTKGRASSBASE)/module/mapcalculator"
            }
            "Создать случайные точки" "" {
                "source $env(TCLTKGRASSBASE)/module/r.random"
            }
            -separator
            "Изменить параметры растра" "" {
                "source $env(TCLTKGRASSBASE)/module/r.resample"
            }
            "IDW-интерполяция по точкам (шир./долг.)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.surf.idw"
            }
            "IDW-интерполяция по точкам (другие коорд.)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.surf.idw2"
            }
            "Интерполяция по растризованным контурам" "" {
                "source $env(TCLTKGRASSBASE)/module/r.surf.contour"
            }
            "Интерполяция растровых карт из контурных линий" "" {
                "source $env(TCLTKGRASSBASE)/module/v.surf.rst"
            }
            -separator
            "Сжать/Открыть сжатый растр" "" {
                "source $env(TCLTKGRASSBASE)/module/r.compress"
            }
        }
        "Выводы" "" {
            "Базовая информация" "" {
                "source $env(TCLTKGRASSBASE)/module/r.info"
            }
            -separator
            "Общая статистика" "" {
                "source $env(TCLTKGRASSBASE)/module/r.stats"
            }
            "Значения и величины категорий" "" {
                "source $env(TCLTKGRASSBASE)/module/r.cats"
            }
            "Диапазон категорий" "" {
                "source $env(TCLTKGRASSBASE)/module/r.describe"
            }
            -separator
            "Площадь" "" {
                "source $env(TCLTKGRASSBASE)/module/r.report"
            }
            "Объем" "" {
                "source $env(TCLTKGRASSBASE)/module/r.volume"
            }
            "Взаимное сочетание категорий (совпадение)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.coin"
            }
            "Величины на трансектах" "" {
                "source $env(TCLTKGRASSBASE)/module/r.profile"
            }
            "Величины на трансектах (исп. азимут и расстояние)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.transect"
            }
            "Матрица корреляции" "" {
                "source $env(TCLTKGRASSBASE)/module/r.covar"
            }
        }
        -separator
        "Обработка снимков" "" $image_processing
    }
    Векторы "Анализ векторных карт" {
        Показать "" $display_vector
        "Анализ" "" {
                "Запрос мышью" "" {
                       "source $env(TCLTKGRASSBASE)/module/d.what.vect"
                }
                "Запрос через файл" "" {
                       "source $env(TCLTKGRASSBASE)/module/v.what"
                }
		"Измерить длину и площадь" "" {
                       "source $env(TCLTKGRASSBASE)/module/d.measure"
                }
        }
        "Преобразовать" "" {
            "в растр" "" {
                "source $env(TCLTKGRASSBASE)/module/v.to.rast"
            }
            "в объекты" "" {
                "source $env(TCLTKGRASSBASE)/module/v.to.sites"
            }
        }
        "Операции" "" {
            "Создать/Перестроить топологию" "" {
                "source $env(TCLTKGRASSBASE)/module/v.support_option=build"
            }
            "Редактировать категории" "" {
                "source $env(TCLTKGRASSBASE)/module/v.support_option=edit"
            }
            "Спрямление линий" "" {
                "source $env(TCLTKGRASSBASE)/module/v.prune"
            }
            "Очистить топологию" "" {
                "source $env(TCLTKGRASSBASE)/module/v.spag"
            }
            "Очистить топологию ASCII карты" "" {
                "source $env(TCLTKGRASSBASE)/module/v.ascii.spag"
            }
            "Удалить мертвые линии" "" {
                "source $env(TCLTKGRASSBASE)/module/v.clean"
            }
            "Удалить минимальные дуги" "" {
                "source $env(TCLTKGRASSBASE)/module/v.trim"
            }
	    "Построить топологию полилиний" "" {
                "source $env(TCLTKGRASSBASE)/module/v.build.polylines"
            } 
            -separator
            "Изменить проекцию ASCII векторов" "" {
                "source $env(TCLTKGRASSBASE)/module/v.proj"
            }
            "Преобразовать координаты ASCII векторов" "" {
                "source $env(TCLTKGRASSBASE)/module/v.transform"
            }
            "Импорт ASCII векторов в GRASS векторы" "" {
                "run v.import &"
            }
            "Экспорт GRASS векторов в ASCII векторы" "" {
                "run v.export &"
            }
            -separator
            "Дигитайзер" "" {
                "run v.digspline &"
            }
            "Создать сетку" "" {
                "source $env(TCLTKGRASSBASE)/module/v.mkgrid"
            }
            "Нарезать (создать новые полигоны)" "" {
                "source $env(TCLTKGRASSBASE)/module/v.cutter"
            }
            "Наложение векторных слоев" "" {
                "source $env(TCLTKGRASSBASE)/module/v.patch"
            }
            "Геометрические вычисления" "" {
                 "source $env(TCLTKGRASSBASE)/module/v.geom"
            }
	    "Интерполяция растровых карт из контурных линий" "" {
                "source $env(TCLTKGRASSBASE)/module/v.surf.rst"
            }
        }
        "Выводы" "" {
            "Базовая информация" "" {
                "source $env(TCLTKGRASSBASE)/module/v.info"
            }
            "Общая статистика" "" {
                "source $env(TCLTKGRASSBASE)/module/v.stats"
            }
            "Размеры" "" {
                "source $env(TCLTKGRASSBASE)/module/v.report"
            }
        }
    }
    Объекты "Анализ карт объектов" {
        Показать "" $display_sites
        "Запрос мышью" "" {
            "source $env(TCLTKGRASSBASE)/module/d.what.sites"
        }
	"Фильтр растром" "" {
	    	"source $env(TCLTKGRASSBASE)/module/s.mask"
	}
	"Средн. значения аттр. объектов по клеткам растра" "" {
	    	"source $env(TCLTKGRASSBASE)/module/s.windavg"
	}
 
        Интерполяция "" {
            "IDW" "" {
                "source $env(TCLTKGRASSBASE)/module/s.surf.idw"
            }
            "Сплайн" "" {
                 "source $env(TCLTKGRASSBASE)/module/s.surf.rst"
            }
        }
	"Преобразовать карту" "" {
            "в растр" "" {
                "source $env(TCLTKGRASSBASE)/module/s.to.rast"
            }
            "в векторы" "" {
                "source $env(TCLTKGRASSBASE)/module/s.to.vect"
            }
        }
        "Выводы" "" {
            "Основная информация" "" {
                "source $env(TCLTKGRASSBASE)/module/s.info"
            }
        }
    }
    Снимки "Обработка снимков" $image_processing
    Импорт "Импорт карт в GRASS" {
        "Растр" "" {
            "ESRI ARC/INFO ASCII-GRID" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.arc"
            }
            "TIFF 8bit" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.tiff"
            }
	    "PNG (24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.png"
            }
            "PPM (24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.ppm"
            }
            "HDF" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.hdf"
            }
            "ERDAS LAN" "" {
                "source $env(TCLTKGRASSBASE)/module/i.in.erdas"
            }
	    "Разные форматы (поддержка GDAL)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.gdal"
            }
            -separator
            "Бинарный файл GTOPO30 в шир/долг регион" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.bin"
            }
            "Бинарный файл GTOPO30 в UTM" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.ll"
            }
        }
        "Векторы" "" {
            "ASCII GRASS векторы" "" {
                "source $env(TCLTKGRASSBASE)/module/v.in.ascii"
            }
            "Файлы из ARC/INFO ungenerate " "" {
                "source $env(TCLTKGRASSBASE)/module/v.in.arc"
            }
	    "ESRI shapefile" "" {
                "source $env(TCLTKGRASSBASE)/module/v.in.shape"
            }
	    "AUTOCAD DXF файлы" "" {
                "source $env(TCLTKGRASSBASE)/module/v.in.dxf"
            }
            "Разные форматы" "" {
                "run v.import &"
            }
	    "Garmin GPS Контр.точки/Маршруты/Отрезки" "" {
		"source $env(TCLTKGRASSBASE)/module/v.in.garmin"
	    }
        }
        "Объекты" "" {
            "ASCII/SPOT файлы" "" {
                "source $env(TCLTKGRASSBASE)/module/s.in.ascii"
            }
            "Garmin GPS Контр.точки/Маршруты/Отрезки" "" {
		"source $env(TCLTKGRASSBASE)/module/s.in.garmin"
	    }
        }
    }
    Экспорт "Экспорт карт из GRASS" {
        "Растр" "" {
            "TIFF (8/24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.tiff"
            }
	    "Binary file" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.bin"
            }
	    "ESRI ARC/INFO ASCII-GRID" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.arc"
            }
            "PPM (24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.ppm"
            }
            "HDF" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.hdf"
            }
	    "GRASS ASCII" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.ascii"
            }
            "ERDAS/LAN" "" {
                "source $env(TCLTKGRASSBASE)/module/i.out.erdas"
            }
            "GRASS CELL в TIFF" "" {
                "source $env(TCLTKGRASSBASE)/module/cell.out.tiff"
            }
        }
        "Векторы" "" {
            "ASCII GRASS векторы" "" {
                "source $env(TCLTKGRASSBASE)/module/v.out.ascii"
            }
	    "ARC/INFO E00 file" "" {
                "source $env(TCLTKGRASSBASE)/module/v.out.e00"
            }
            "В файл ARC/INFO ungenerate file" "" {
                "source $env(TCLTKGRASSBASE)/module/v.out.arc"
            }
            "Разные форматы" "" {
                "run v.export &"
            }
        }
        "Объекты" "" {
            "ASCII файл" "" {
                "source $env(TCLTKGRASSBASE)/module/s.out.ascii"
            }
        }
    }
    СоздКарт "Создание Карт" {
        "Драйвер paint" "" {
            "Создать/ред. иконки" "" {
                "run p.icons &"
            }
            "Создать/ред. метки" "" {
                "run p.labels &"
            }
            "Показать метки" "" {
                "source $env(TCLTKGRASSBASE)/module/d.paint.labels"
            }
            "Выбор принтера" "" {
                "source $env(TCLTKGRASSBASE)/module/p.select"
            }
            "Создание карты paint" "" {
                "source $env(TCLTKGRASSBASE)/module/p.map.new"
            }
        }
        "PostScript" "" {
            "Создать/ред. иконки" "" {
                 "source $env(TCLTKGRASSBASE)/module/ps.icon"
             }
             "Выбор принтера" "" {
                 "source $env(TCLTKGRASSBASE)/module/ps.select"
             }
             "Создание карты PostScript" "" {
                 "source $env(TCLTKGRASSBASE)/module/ps.map"
             }
        }
        "Xfig (внешн.)" "" {
            "run xfig&"
        }
    }
    БД "Базы данных" {
        "PostgreSQL" "" {
            "Общие" "" {
                "Выбор DB" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.select.pg"
                }
    	    "Список таблиц" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.table.pg"
                }
            "Список столбцов" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.column.pg"
                }
	    "Статистика" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.stats.pg"
                }
            }
            "Запрос мышью" "" {
                "векторы" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.what.v.pg"
                }
	        "объекты" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.what.s.pg"
                }
	        "растр" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.what.r.pg"
                }
            }
	    "Показать" "" {
                "векторы" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.vect.pg"
                }
	        "объекты" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.site.pg"
                }
	        "растр" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.rast.pg"
                }
	    }
            "Рекласс векторов" "" {
                    "source $env(TCLTKGRASSBASE)/module/v.reclass.pg"
            }   
        }
	-separator
	"DBMI" "" {
            "Выбрать драйвер" "" {
                "source $env(TCLTKGRASSBASE)/module/db.connect.driver"
            }
            "Соединиться" "" {
                "source $env(TCLTKGRASSBASE)/module/db.connect"
            }
            -separator
	    "Список таблиц" "" {
                "source $env(TCLTKGRASSBASE)/module/db.tables"
            }	    	    
            "Список столбцов" "" {
                "source $env(TCLTKGRASSBASE)/module/db.columns"
            }
            "Описание таблиц" "" {
                "source $env(TCLTKGRASSBASE)/module/db.describe"
            }
            -separator
            "Выбрать все" "" {
                "source $env(TCLTKGRASSBASE)/module/db.select.all"
            }	    
            "Выбрать" "" {
                "source $env(TCLTKGRASSBASE)/module/db.select"
            } 
            "Выполнить" "" {
                "source $env(TCLTKGRASSBASE)/module/db.execute"
            }
	    -separator
            "Рекласс векторов" "" {
                "source $env(TCLTKGRASSBASE)/module/v.db.reclass"
            }
            "Загрузить векторы в БД" "" {
                "source $env(TCLTKGRASSBASE)/module/v.to.db"
            }	    	    
        }
    }
    Разное "Разные преобразования" $misc
    Помощь Помощь {
        "Модули" "" {
           "source $env(TCLTKGRASSBASE)/module/g.manual"
        }
        -separator
        "Помощь" "" {
           "source $env(TCLTKGRASSBASE)/main/help.tcl"
        }
        "О программе" "" {
           "source $env(TCLTKGRASSBASE)/main/about.tcl"
        }
	"О GRASS" "" {
           "source $env(TCLTKGRASSBASE)/main/grassabout.tcl"
        }
	-separator
	"Об этой системе" "" {
	   "exec $env(TCLTKGRASSBASE)/main/tksys.tcl --tcltk"
	}
	-separator
	"Помощь по скриптам" "" {
	    "source $env(TCLTKGRASSBASE)/main/help-scripting.tcl"
	}
	"Помощь по html-browser" "" {
	    "source $env(TCLTKGRASSBASE)/main/help-netscape.tcl"
	}
    }
    Выйти Пока! quit
}
