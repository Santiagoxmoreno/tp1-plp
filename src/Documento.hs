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

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc base fTexto fLinea (Vacio) = base
foldDoc base fTexto fLinea (Texto str doc) = fTexto str (foldDoc base fTexto fLinea doc)
foldDoc base fTexto fLinea (Linea n doc) = fLinea n (foldDoc base fTexto fLinea doc)

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 fTexto Linea d1 -- si d1 es una linea, se le agrega d2 al final
            where fTexto txt (Texto str doc) = Texto (txt ++ str) doc -- si son dos textos se concatenan en uno, asi se mantiene el invariante
                  fTexto txt doc = Texto txt doc  -- si no son dos textos, simplemente se juntan

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio (Texto) (\x rec -> Linea (x + i) rec) --no modifica al texto, y si la linea no es negativa, entonces indentar no la va a hacer negativa, porque le suma un int mayor a 0

mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\x rec -> "\n" ++ (replicate x ' ') ++ rec)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
