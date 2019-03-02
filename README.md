# Logica-proposicional-en-haskell
Programa en Haskell que permite realizar algunas operaciones de la lógica proposicional.
Dentro del código hay incluidas varias formas proposicionales precargadas en las que se observa la sintaxis.

Se dispone de las siguientes operaciones así como un comando de ayuda:
* vars:  'vars f'  calcula una lista con los nombres de las variables proposicionales que hay en f sin repeticiones (aunque el orden es irrelevante).
* tautologia:  'tautolofia f'  reconoce si f es o no una tautologia, es decir, si es cierta para valores de verdad cualesquiera de sus variables proposicionales.
* satisfactible:  'satisfactible f'  reconoce si la formula f es satisfactible o no, es decir si es cierta para algunos valores de verdad de sus variables proposicionales.
* consecuencia:  'consecuencia f1 f2'  reconoce si una formula f1 es consecuencia logica de otra f2, es decir, si para valores de verdad cualesquiera de las variables proposicionales, cuando f2 es cierta lo es f1.
* equivalente:  'equivalente f1 f2'  reconoce si una formula f1 es equivalente a otra f2, es decir, si para valores de verdad cualesquiera de las variables proposicionales, cuando f2 es cierta lo es f1 y viceversa.
* consecuencias:  'consecuencias fs'  dada una lista de formulas fs, nos devolvera una lista con cada formula f de fs emparejada con una listade aquellas formulas de fs que son consecuencia logica de f"
* equivalentes:  equivalentes fs  dada una lista de formulas fs, nos devuelve el conjunto cociente de fs por la relacion de equivalencia logica, es decir, una particion de fs en sublistas, cada una de las cuales esta formada por formulas de fs equivalentes entre si.
