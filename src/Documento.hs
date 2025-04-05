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
d1 <+> d2 = foldDoc d2 fTexto fLinea d1
            where fTexto = (\text documento -> if verificarTexto(documento) then concatenarTextos text documento else (Texto text documento))
                  fLinea = (\n documento -> if verificarTexto(documento) then Linea n (documento) else lineaConLinea n documento)
concatenarTextos :: String -> Doc -> Doc
concatenarTextos str (Texto str2 documento) = Texto (str ++ str2) documento

lineaConLinea :: Int -> Doc -> Doc
lineaConLinea n1 (Linea n2 doc2) = Linea (n1 + n2) doc2

textoAtexto :: String -> Doc -> Doc
textoAtexto str (Texto str2 doc2) = Texto (str ++ str2) doc2  

verificarTexto :: Doc -> Bool 
verificarTexto (Texto _ _) = True
verificarTexto _ = False

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio (\x rec -> Texto x rec) (\x rec -> Linea (x + i) rec)

mostrar :: Doc -> String
mostrar = foldDoc "" (\x rec -> x ++ rec) (\x rec -> "\n" ++ nEspacios x ++ rec)
          where nEspacios = foldr (\x rec n -> if n == 0 then [] else " "++rec(n-1)) (const []) (repeat " ")

--[1,2,3]
--f 1 (f 2 (f 3 base))

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
