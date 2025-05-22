module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico (TextoPP _) = True
pponAtomico (IntPP _) = True
pponAtomico _ = False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP lista) = all (pponAtomico . snd) lista
pponObjetoSimple _ = False

intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = texto ""
intercalar s lista = foldr1 (\x rec -> x <+> s <+> rec) lista

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\x rec -> texto x <+> rec) (\x rec -> texto " " <+> rec)

{-
Consideramos que la función pponADoc usa recursion primitiva, ya que se accede y hacemos uso de la subestructura,
es decir, el PPON dentro de ObjetoPP, en esta parte del codigo: (if pponAtomico (snd $ head lista)). Luego, la llamada 
recursiva que se hace en aux, seria estructural: (<+> pponADoc pp). Entonces, como la recursion primitiva contiene a 
la recursion estructural, decimos que la funcion usa recursion primitiva.
-}

pponADoc :: PPON -> Doc
pponADoc (TextoPP str) = texto (show str)
pponADoc (IntPP n) = texto (show n)
pponADoc (ObjetoPP lista) = (if pponObjetoSimple $ ObjetoPP lista then aplanar else id) $ entreLlaves $ map aux lista
                            where aux (str, pp) = texto (show str ++ ": ") <+> pponADoc pp

-- En el `pponADoc`, solo verifican si el primer objeto dentro de la lista
-- es atómico en lugar de verificar si todo el ppon es simple. Con lo cual no resuelven algunos casos.
