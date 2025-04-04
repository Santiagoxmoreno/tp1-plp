module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where
 
data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

--foldr f z [] = z
--foldr f z (x : xs) = f x (foldr f z xs)

-- foldDoc :: ... PENDIENTE: Ejercicio 1 ...
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc base fTexto fLinea (Vacio) = base
foldDoc base fTexto fLinea (Texto str doc) = fTexto str (foldDoc base fTexto fLinea doc)
foldDoc base fTexto fLinea (Linea n doc) = fLinea n (foldDoc base fTexto fLinea doc)

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(Vacio) <+> (Vacio) = Vacio
(Texto str1 _) <+> (Texto str2 _) = Texto (str1 ++ str2) Vacio

indentar :: Int -> Doc -> Doc
indentar i = error "PENDIENTE: Ejercicio 3"

mostrar :: Doc -> String
mostrar = error "PENDIENTE: Ejercicio 4"

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
