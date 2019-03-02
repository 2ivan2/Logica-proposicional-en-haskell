--Practica programacion declarativa Haskell

--Objetivo de la practica: Escribir un programa en Haskell que permita realizar algunas operaciones
-- con formulas de la loogica proposicional.

--Nombre de variables
type Var = String
--Data para formulas proposicionales
data FProp = V Var | No FProp | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp deriving (Read)

--Instancias
instance Show FProp where
    show(V var) = var
    show(No var) = "¬" ++ show var 
    show(Y var1 var2) = "(" ++ show(var1) ++ " /\\ " ++ show(var2) ++ ")"
    show(O var1 var2) = "(" ++ show(var1) ++ " \\/ " ++ show(var2) ++ ")"
    show(Si var1 var2) = "(" ++ show(var1) ++ " -> " ++ show(var2) ++ ")"
    show(Sii var1 var2) =  "(" ++ show(var1) ++ " <-> " ++ show(var2) ++ ")"

instance Eq FProp where
    V var1 == V var2 = var1 == var2
    No fprop1 == No fprop2 = fprop1 == fprop2
    (Y fprop1 fprop2) == (Y fprop3 fprop4) = ((fprop1 == fprop3) && (fprop2 == fprop4)) || ((fprop1 == fprop4) && (fprop2 == fprop3))
    (O fprop1 fprop2) == (O fprop3 fprop4) = ((fprop1 == fprop3) && (fprop2 == fprop4)) || ((fprop1 == fprop4) && (fprop2 == fprop3))
    (Si fprop1 fprop2) == (Si fprop3 fprop4) = ((fprop1 == fprop3) && (fprop2 == fprop4))
    (Sii fprop1 fprop2) == (Sii fprop3 fprop4) = ((fprop1 == fprop3) && (fprop2 == fprop4)) || ((fprop1 == fprop4) && (fprop2 == fprop3))
    x == y = False  --Si el ajuste de patrones no ha servido con los anteriores casos estamos intentando igualar dos constructores distintos
instance Ord FProp where
    x <= x' = consecuencia x' x

--Formulas ejemplos
f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))
f2 = O (V "p") (Si (No (V "q")) (No (V "p")))
f3 = Y (V "p") (Y (V "q") (O (No (V "q")) (V "r")))
f4 = Si (Y (V "p") (V "q")) (V "p")
f5 = No (Y (V "p") (O (V "p") (V "r")))
parar = V "empty"     --Usado para la interfaz grafica

--Tablas verdad si y sii (puestas con formulas equivalentes dadas por || y not)
si :: Bool -> Bool -> Bool
si x y = (not x) || y 

sii :: Bool -> Bool -> Bool
sii x y = (si x y) && (si y x)
--------------------------------------------------------------------
vars:: FProp -> [Var]
vars f = varsAux f []

varsAux:: FProp -> [Var] -> [Var]
varsAux (V var) l = insertarSinRepetidosElem l var
varsAux (No fprop) l = varsAux fprop l
varsAux (Y fprop1 fprop2) l = varsAux fprop2 ((varsAux fprop1 l))
varsAux (O fprop1 fprop2) l = varsAux fprop2 ((varsAux fprop1 l))
varsAux (Si fprop1 fprop2) l = varsAux fprop2 ((varsAux fprop1 l))
varsAux (Sii fprop1 fprop2) l = varsAux fprop2 ((varsAux fprop1 l))

--Insertar un elemento en una lista si no esta
insertarSinRepetidosElem :: (Eq a) => [a] -> a -> [a]
insertarSinRepetidosElem l a
    |notElem a l = l ++ [a]
    |otherwise = l
--------------------------------------------------------------------
--Para las valoraciones tomaremos un array que contenga una var concreta si la queremos poner a True y no la contenga para ponerla a False. 
type Valoracion = [Var]
--Nos calculara las posibles evaluaciones para una formula con n variables (esto es el conjunto partes de el conjunto formado por las variables de la formula 
--proposicional, siendo util ya que para cada conjunto de los contenidos en este conjunto partes, cada variable que tenga equivaldra a hacer una sustitucion 
--de esa variable por true, y si no esta por un false, mediante el elem de listas que nos detectara si ESTA o NO ESTA un elemento)
valoraciones:: Valoracion -> [Valoracion]
valoraciones [] = [[]]
valoraciones (x:xs) = let valoraciones_xs = (valoraciones xs) in valoraciones_xs ++ [(x:i) | i<- valoraciones_xs]
        --Let utilizado para ahorrar calculos 

evaluacion :: FProp -> Valoracion -> Bool
evaluacion (V var) v = elem var v
evaluacion (No fprop) v = not $ evaluacion fprop v
evaluacion (Y fprop1 fprop2) v = (evaluacion fprop1 v) && (evaluacion fprop2 v)
evaluacion (O fprop1 fprop2) v = (evaluacion fprop1 v) || (evaluacion fprop2 v)
evaluacion (Si fprop1 fprop2) v = si (evaluacion fprop1 v) (evaluacion fprop2 v)
evaluacion (Sii fprop1 fprop2) v = sii (evaluacion fprop1 v) (evaluacion fprop2 v)
--------------------------------------------------------------------
tautologia :: FProp -> Bool
tautologia f = notElem False [(evaluacion f x) | x <- (valoraciones $ vars f)]
--------------------------------------------------------------------
satisfactible :: FProp -> Bool
satisfactible f = elem True [(evaluacion f x) | x <- (valoraciones $ vars f)]
--------------------------------------------------------------------
--fprob1 es consecuencia logica de fprob2 si para las valoraciones en las que fprop2 es cierto, lo es fprop1
consecuencia :: FProp -> FProp -> Bool
consecuencia fprop1 fprop2 = all (== True) (zipWith (si) [(evaluacion fprop2 x) | x <- (valoraciones $ vars fprop2)] [(evaluacion fprop1 x) | x <- (valoraciones $ vars fprop1)])
--Lo que hago es comprobar que cuando un elemento de fprop2 es cierto lo tiene que ser su asociado en la tabla de verdad de fprop1
--------------------------------------------------------------------
equivalente :: FProp -> FProp -> Bool
equivalente fprop1 fprop2 = [(evaluacion fprop1 x) | x <- (valoraciones $ vars fprop1)] == [(evaluacion fprop2 x) | x <- (valoraciones $ vars fprop2)]
--Para ser equivalentes tienen que tener la misma tabla de verdad
--------------------------------------------------------------------
consecuencias :: [FProp] -> [(FProp, [FProp])]
consecuencias x = [(i, (filter (`consecuencia` i) x)) | i <- x]

--------------------------------------------------------------------
equivalentes :: [FProp] -> [[FProp]]
equivalentes arrayfprop = map (eliminarRepetidosAux []) (eliminarRepetidosAux [] [[j | j <- (filter (equivalente i) arrayfprop)] | i <- arrayfprop])
--equivalentes filtra las formulas equivalentes a una dada. Los eliminarRepetidosAux se encargan de que no haya elementos iguales ni en la lista ni en sus componentes (ya que se trata de listas de listas)
--En la lista para tener una unica vez la clase de equivalencia, y en los elementos de la lista para que no se incluya más de una vez la misma formula

--Recibe una lista vacia como primer argumento y otra como segundo, y guarda en la primera la segunda sin elementos repetidos
eliminarRepetidosAux :: (Eq a) => [a] -> [a] -> [a]
eliminarRepetidosAux _ [] = []
eliminarRepetidosAux ls (x:xs)
    |elem x ls = eliminarRepetidosAux ls xs
    |otherwise = x : eliminarRepetidosAux (x:ls) xs
--------------------------------------------------------------------
main :: IO ()
main = do mostrarMenu
          opcion <- getInt
          cachearOpcion opcion
          if opcion /= 0 then main
            else return ()

mostrarMenu :: IO ()
mostrarMenu = do     
    putStrLn "\nBienvenido a Logica-Proposicional 2.0"
    putStrLn "---------------------------------------"
    putStrLn "Introduzca el numero asociado a la operacion que desea realizar:"
    putStrLn "1.- vars"
    putStrLn "2.- tautologia"
    putStrLn "3.- satisfactible"
    putStrLn "4.- consecuencia"
    putStrLn "5.- equivalente"
    putStrLn "6.- consecuencias"
    putStrLn "7.- equivalentes"
    putStrLn "8.- ayuda"
    putStrLn "0.- salir"
    putStrLn "---------------------------------------"
    putStr "Introduzca una opcion: "

cachearOpcion :: Int -> IO ()
cachearOpcion x
    |x == 0 = do putStrLn "Hasta luego...."
                 putStrLn "Fin de la sesion..."
    |x == 1 = do putStr "Introduzca una formula proposicional: "
                 fprop <- getFProp
                 putStrLn(show(vars fprop))
    |x == 2 = do putStr "Introduzca una formula proposicional: "
                 fprop <- getFProp
                 putStrLn(show(tautologia fprop))
    |x == 3 = do putStr "Introduzca una formula proposicional: "
                 fprop <- getFProp
                 putStrLn(show(satisfactible fprop))
    |x == 4 = do putStr "Introduzca la primera formula proposicional: "
                 fprop1 <- getFProp
                 putStr "Introduzca la segunda formula proposicional: "
                 fprop2 <- getFProp
                 putStrLn(show(consecuencia fprop1 fprop2))
    |x == 5 = do putStr "Introduzca la primera formula proposicional: "
                 fprop1 <- getFProp
                 putStr "Introduzca la segunda formula proposicional: "
                 fprop2 <- getFProp
                 putStrLn(show(equivalente fprop1 fprop2))
    |x == 6 = do putStr "Introduzca formula proposicional (introduzca la palabra 'parar' para finalizar): "
                 fprops <- getFProps
                 putStrLn(show(consecuencias fprops))
    |x == 7 = do putStr "Introduzca formula proposicional (introduzca la palabra 'parar' para finalizar): "
                 fprops <- getFProps
                 putStrLn(show(equivalentes fprops))
    |x == 8 = do ayuda
    |otherwise =  do putStrLn "Introduzca una opcion corecta"

ayuda:: IO ()
ayuda = do
    putStrLn "1.-vars:  'vars f'  calcula una lista con los nombres de las variables proposicionales que hay en f sin repeticiones (aunque el orden es irrelevante)."
    putStrLn "2.-tautologia:  'tautolofia f'  reconoce si f es o no una tautologia, es decir, si es cierta para valores de verdad cualesquiera de sus variables proposicionales"
    putStrLn "3.-satisfactible:  'satisfactible f'  reconoce si la formula f es satisfactible o no, es decir si es cierta para algunos valores de verdad de sus variables proposicionales"
    putStrLn "4.-consecuencia:  'consecuencia f1 f2'  reconoce si una formula f1 es consecuencia logica de otra f2, es decir, si para valores de verdad cualesquiera de las variables proposicionales, cuando f2 es cierta lo es f1"
    putStrLn "5.-equivalente:  'equivalente f1 f2'  reconoce si una formula f1 es equivalente a otra f2, es decir, si para valores de verdad cualesquiera de las variables proposicionales, cuando f2 es cierta lo es f1 y viceversa"
    putStrLn "6.-consecuencias:  'consecuencias fs'  dada una lista de formulas fs, nos devolvera una lista con cada formula f de fs emparejada con una listade aquellas formulas de fs que son consecuencia logica de f"
    putStrLn "7.-equivalentes:  equivalentes fs  dada una lista de formulas fs, nos devuelve el conjunto cociente de fs por la relacion de equivalencia logica, es decir, una particion de fs en sublistas, cada una de las cuales esta formada por formulas de fs equivalentes entre si"

getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

--Para facilitar la ejecucion se han incluido f1, f2, f3, f4, f5 en la lectura de formulas proposicionales
getFProp:: IO FProp
getFProp = do line <- getLine
              if line == "f1" then return f1
                            else if (line == "f2") then return f2
                            else if (line == "f3") then return f3
                            else if (line == "f4") then return f4
                            else if (line == "f5") then return f5
                            else if (line == "parar") then return parar
                            else return (read line::FProp)

--Obtiene formulas hasta que le das una que que le pasas V "empty", en nuestro caso hemos hecho para que con que le escribas parar le pase el esa formula y pare
getFProps:: IO [FProp]
getFProps = do fprop <- getFProp
               if fprop == V "empty" then return []
                                     else do putStr "Introduzca formula proposicional (introduzca la palabra 'parar' para finalizar): "
                                             fp <-getFProps
                                             return (fprop:fp)
