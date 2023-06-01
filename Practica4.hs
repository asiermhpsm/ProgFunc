import Data.Char (isAlpha, toLower)
import Data.List

{-
Funcionamiento general del codigo:
Primero se generarán toda las combinaciones posibles con las letras disponibles. A continuación se eliminaran las que no cumplan unas características generales (como que no es posible que haya 4 consonantes seguidas)
y luego se comprobará que cumplan otras reglas relacionadas a las sílabas (como que consonantes al final de sílaba solo puede ser l, n, s,r).
Estas reglas de las silabas se harán en profundiad, es decir que dada la palabra se intentara separar en sílabas
de todas la maneras posibles de manera que cumplan con las reglas llegando al final de una separación antes de probar con otra y si encuentra una separación que satisface las reglas no se busca más.
Reglas generales:
-No puede haber 4 consonantes seguidas
...........
Reglas de sílabas:
-consonantes al final de sílaba solo puede ser l, n, s,r
..........
-}

--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--MAIN
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--
main :: IO ()
main = do
  putStrLn "Este programa facilita el juego del Scrable. Se pedirá al usuario que ingrese las letras que tiene disponibles, así como las letras disponibles en el tablero y se le devolverá las palabras que puede formar. "
  putStrLn "Tenga en cuenta que de lo que escriba el usuario solo se tendrán en cuenta las letras."
  loop

-- Bucle infinito
loop :: IO ()
loop = do
  putStrLn "\n\nIngrese sus letras (o escriba 'salir' para finalizar):"
  input1 <- getLine
  if input1 /= "salir"
    then do
      putStrLn "Ingrese las letras del tablero:"
      input2 <- getLine
      imprimeRes (soloLetrasMinusculas (input1 ++ input2))
      loop
    else putStrLn "\nSaliendo...."

-- Dado un string con las letras imprime los resultados
imprimeRes :: String -> IO ()
imprimeRes str = do
  let (resultado, n) = generaRes (fromIntegral (length str)) str
  if null resultado
    then putStrLn "No se han podido encontrar palabras"
    else do
      putStrLn "Aquí tienes algunas palabras que puedes usar:"
      putStrLn $ intercalate ", " resultado
      masRes (n - 1) str

-- Genera mas resultados si así lo desea el usuario
masRes :: Integer -> String ->  IO ()
masRes n str =
  if n == 1
    then return()
    else do
      putStrLn "\n¿Quieres más resultados? (escribe 'si' o 's' si es el caso)"
      input <- getLine
      if input == "si" || input == "s"
        then do
          let (resultado, n') = generaRes n str
          if null resultado
            then putStrLn "\nNo se han podido encontrar mas palabras"
            else do
              putStrLn "\nAquí tienes algunas palabras que puedes usar:"
              putStrLn $ intercalate ", " resultado
              masRes (n' - 1) str
        else return()


--Genera todos los resultados posibles se cierta longitud con la maxima longitud posible
generaRes :: Integer -> String ->  ([String], Integer)
generaRes 1 _ = ([], 1)
generaRes n str = let res = procesarNum n str
                  in if null res
                    then generaRes (n - 1) str
                    else (res, n)


--Dada una lista de letras te da todas las posibles palabras validas de longitud n
procesarNum :: Integer -> String ->  [String]
procesarNum n str = sustListaALetras(filter palabraValida (sustListaACod (generaPalabrasNum n str)))

--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--FUNCIONES AUXILIARES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--

--Devuelve true si es consonante
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiou"

--Devuelve true si es consonante
esConsonante :: Char -> Bool
esConsonante c = not (esVocal c) && (c `elem` ['a'..'z'] || c `elem` ['1', '2'])

--Dado un string devuelve solo las letras y convertidas todas a minuscula
soloLetrasMinusculas :: String -> String
soloLetrasMinusculas str = map toLower (filter isAlpha str)

--Sustituye letras especiales (como ch o ll) por un codigo
sustLetrasACod :: String -> String
sustLetrasACod [] = []
sustLetrasACod [x] = [x]
sustLetrasACod (x:y:res)
                    | x == 'c' && y == 'h' = ('1':sustLetrasACod res)
                    | x == 'l' && y == 'l' = ('2':sustLetrasACod res)
                    | otherwise = (x:sustLetrasACod (y:res))

--Sustituye todas las palabras de una lista segun la funcion sustLetras
sustListaACod :: [String] -> [String]
sustListaACod = map sustLetrasACod

--Sustituye codigo por sus respectivas letras especiales (como ch o ll)
sustCodALetras :: String -> String
sustCodALetras [] = []
sustCodALetras [x] = [x]
sustCodALetras (x:res)
                    | x == '1' = ('c':'h':sustCodALetras res)
                    | x == '2' = ('l':'l':sustCodALetras res)
                    | otherwise = (x:sustCodALetras res)

--Sustituye todas las palabras de una lista segun la funcion sustLetras
sustListaALetras :: [String] -> [String]
sustListaALetras = map sustCodALetras


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--GENERACION DE POSIBLES PALABRAS
----------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Genera todas las palabras posibles
generaPalabrasNum :: Integer -> String -> [String]
generaPalabrasNum n str = nub $ permutaLetras(combinacionesNum n str)

--Calcula las permutaciones de todas las palabras de la lista
permutaLetras :: [String] -> [String]
permutaLetras strs = nub $ concatMap (\str -> permutations str) strs

--Devuelve todas las combinaciones posibles de cierta longitud
combinacionesNum :: Integer -> String -> [String]
combinacionesNum = combinaciones_2

-- 2ª definición
combinaciones_2 :: Integer -> [a] -> [[a]]
combinaciones_2 0 _          = [[]]
combinaciones_2 _ []         = []
combinaciones_2 k (x:xs) =
    [x:ys | ys <- combinaciones_2 (k-1) xs] ++ combinaciones_2 k xs


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--VALIDAR PALABRA
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--A partir de ahora se supone correcto que el parametro de entrada es una lista de letras en minuscula, sin números ni ningún tipo de simbolo

--Devuelve true si la palabra es valida con las reglas definidas
palabraValida :: String -> Bool
palabraValida str = palabraValidaGeneral str && palabraValidaPorSilabas str

--Realiza comprobaciones generales sobre la palabra y devuelve si es valida
palabraValidaGeneral :: String -> Bool
palabraValidaGeneral str =
  let
    funciones = [noMasTresConsonantesSeguidas, qMasUMasConsonante, hMasVocal, noConsMasH,dosLetrasIguales,noTerminaDosConsonantes,noMasTresVocalesSeguidas,noCaracEspMasVocal]  -- Lista de funciones de reglas generales a comprobar TODO
  in
    all (\f -> f str) funciones

--Hace un busqueda en profundidad para ver si una palabra es valida separandola por silabas y comprobando cada silaba, supone silabas de hasta 4 letras
palabraValidaPorSilabas :: String -> Bool
palabraValidaPorSilabas [] = True
palabraValidaPorSilabas (x:y:z:w:resto) = (silabaValida [x, y, z, w] && palabraValidaPorSilabas resto)
                                                            || (silabaValida [x, y, z] && palabraValidaPorSilabas (w:resto))
                                                            || (silabaValida [x, y] && palabraValidaPorSilabas (z:w:resto))
                                                            || (silabaValida [x] && palabraValidaPorSilabas (y:z:w:resto))
palabraValidaPorSilabas (x:y:z:resto) = (silabaValida [x, y, z]   &&      palabraValidaPorSilabas resto)
                                                            || (silabaValida [x, y]     &&      palabraValidaPorSilabas (z:resto))
                                                            || (silabaValida [x]       &&      palabraValidaPorSilabas (y:z:resto))
palabraValidaPorSilabas (x:y:resto) = (silabaValida [x, y]        &&      palabraValidaPorSilabas resto)
                                                            || (silabaValida [x]   &&      palabraValidaPorSilabas (y:resto))
palabraValidaPorSilabas (x:resto) = silabaValida [x]      &&      palabraValidaPorSilabas resto


--Comprueba si una silaba es valida, supone que las silabas son de maximo 4 letras
silabaValida :: String -> Bool
silabaValida [x] =   let funciones = [esVocal]  -- Solo las vocales pueden ser sílabas solas
                              in all (\f -> f x) funciones
silabaValida str@[_, _] = let funciones = [consonanteFinal,cv_vc]  -- Lista de funciones de reglas de silabas de longitud 2 a comprobar TODO
                                      in all (\f -> f str) funciones
silabaValida str@[_, _, _] = let funciones = [consonanteFinal,dosConsonantesInicio]  -- Lista de funciones de reglas de silabas de longitud 3 a comprobar TODO
                                          in all (\f -> f str) funciones
silabaValida str@[_, _, _, _] = let funciones = [consonanteFinal,dosConsonantesInicio,ccvv]  -- Lista de funciones de reglas de silabas de longitud 4 a comprobar TODO
                                              in all (\f -> f str) funciones
silabaValida _ = False


--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--REGLAS GENERALES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Todas la reglas devuelven false si no se cumple la regla del español que se esta implementado

--Devuelve false si encuentra tres o mas consonantes seguidas
noMasTresConsonantesSeguidas :: String -> Bool
noMasTresConsonantesSeguidas str = not (hayTresConsonantesSeguidas str)
  where
    hayTresConsonantesSeguidas [] = False
    hayTresConsonantesSeguidas (x:y:z:xs) | esConsonante x && esConsonante y && esConsonante z = True
                                          | otherwise = hayTresConsonantesSeguidas (y:z:xs)
    hayTresConsonantesSeguidas _ = False

-- Devuelve False si encuentra alguna 'q' que no esté seguida de una 'u' y una vocal
qMasUMasConsonante :: String -> Bool
qMasUMasConsonante [] = True
qMasUMasConsonante [x] = x /= 'q'
qMasUMasConsonante [x, y] = x /= 'q' && qMasUMasConsonante [y]
qMasUMasConsonante (x:y:z:res) = if x == 'q'
                                   then y == 'u' && esVocal z && qMasUMasConsonante (y:z:res)
                                   else qMasUMasConsonante (y:z:res)

--Devuelve false si hay alguna h que no este seguida de vocal
hMasVocal :: String -> Bool
hMasVocal [] = True
hMasVocal [x] = x /= 'h'
hMasVocal (x:y:res) = if x == 'h'
                    then esVocal y && hMasVocal (y:res)
                    else hMasVocal (y:res)

--Devuelve false si hay alguna h que este precedida con una consonante
noConsMasH :: String -> Bool
noConsMasH [] = True
noConsMasH [_] = True
noConsMasH [x, y] = if y == 'h' then not(esConsonante x) else True
noConsMasH (x:y:res) = if y == 'h' then not(esConsonante x) && noConsMasH (y:res) else noConsMasH (y:res)

--Devuelve false si encuentra dos letras seguidas iguales (distintas de rr)
dosLetrasIguales :: String -> Bool
dosLetrasIguales str = not (hayDosLetrasIguales str)
  where
    hayDosLetrasIguales [] = False
    hayDosLetrasIguales (x:y:xs) | (x==y) && (x /= 'r')= True
                                          | otherwise = hayDosLetrasIguales (y:xs)
    hayDosLetrasIguales _ = False

--Devuelve false si encuentra tres vocales seguidas que no sean de la forma 'i'x'i' (correspondientes a verbos como correriais)
noMasTresVocalesSeguidas :: String -> Bool
noMasTresVocalesSeguidas str = not (hayTresVocalesSeguidas str)
  where
    hayTresVocalesSeguidas [] = False
    hayTresVocalesSeguidas (x:y:z:xs) |(esVocal x && esVocal y && esVocal z) && ((x /= 'i') || (z /= 'i')) = True
                                          | otherwise = hayTresVocalesSeguidas (y:z:xs)
    hayTresVocalesSeguidas _ = False

--Devuelve false si termina en dos consonantes
noTerminaDosConsonantes :: String -> Bool
noTerminaDosConsonantes str = case reverse str of
                               c1:c2:_ -> not (esConsonante c1 && esConsonante c2)
                               _ -> True


--Devuelve false si los caracteres especiales ch o ll no estan precedidos de una vocal
noCaracEspMasVocal :: String -> Bool
noCaracEspMasVocal [] = True
noCaracEspMasVocal [x] = True
noCaracEspMasVocal (x:y:res) = if x == '1' || x == '2'
                                  then esVocal y && noCaracEspMasVocal res
                                  else noCaracEspMasVocal (y:res)



--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--REGLAS DE SILABAS
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Todas la reglas devuelven false si no se cumple la regla del español que se esta implementado

--REGLAS DE SILBA GENERALES
-------------------------------

--Devuelve false si la ultima letra es consonante y distinta de l,n,s,r
consonanteFinal :: String -> Bool
consonanteFinal str = let ult = last str in not(esConsonante ult && not(ult `elem` "lnsr"))

--Devuelve false si encuentra dos consonantes al inicio que la segunda no sean 'l' o 'r' o la primera no sea una consonante fuerte (b, c, d, f, g, p, t)
dosConsonantesInicio :: String -> Bool
dosConsonantesInicio (c1:c2:_) = if esConsonante c1 && esConsonante c2
                                    then c1 `elem` "bcdfgpt" && c2 `elem` "lr"
                                    else True
dosConsonantesInicio _ = False

--REGLAS DE SILABAS DE 2 LETRAS
-------------------------------

--Devuelve false si se le pasa una silaba de longitud dos letras o si no son consonante-vocal o vocal-consonante
cv_vc :: String -> Bool
cv_vc [x,y] = (esConsonante x && esVocal y) || (esVocal x && esConsonante y)
cv_vc _ = False

--REGLAS DE SILABAS DE 3 LETRAS
-------------------------------


--REGLAS DE SILABAS DE 4 LETRAS
-------------------------------

--Devuelve false si se le pasa una silaba de longitud 4 letras o si no son consonante-consonante-vocal-vocal
ccvv :: String -> Bool
ccvv [x,y,z,w] = esConsonante x && esConsonante y && esVocal z && esVocal w
ccvv _ = False

