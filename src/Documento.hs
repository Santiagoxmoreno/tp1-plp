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
d1 <+> d2 = foldDoc d2 fTexto fLinea d1
            where fTexto txt doc = if esTexto(doc) then concatenarTextos txt doc else (Texto txt doc) -- si son textos se concatenan, y si es linea o vacio se ponen al final
                  fLinea n doc = if esTexto(doc) then Linea n (doc) else lineaConLinea n doc
                  concatenarTextos str (Texto str2 doc) = Texto (str ++ str2) doc --la concatenacion no puede producir string vacio, ni salto de linea, si ambos cumplen el invariante
                  lineaConLinea n1 (Linea n2 doc2) = Linea n1 (Linea n2 doc2) --no se modifican las i de las lineas que estamos concatenando, entonces si no son menores a 0 el resultado tampoco
                  esTexto (Texto _ _) = True
                  esTexto _ = False

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
