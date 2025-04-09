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
pponObjetoSimple (ObjetoPP lista) = foldr (&&) True (map (pponAtomico . snd) lista)
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

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

{-
Consideramos que la función pponADoc usa recursion primitiva, ya que se accede y hacemos uso de la subestructura,
es decir, el PPON dentro de ObjetoPP, en esta parte del codigo: (if pponAtomico (snd $ head lista)). Luego, la llamada 
recursiva que se hace en aux, seria estructural: (<+> pponADoc pp). Entonces, como la recursion primitiva contiene a 
la recursion estructural, decimos que la funcion usa recursion primitiva.
-}

pponADoc :: PPON -> Doc
pponADoc (TextoPP str) = texto (show str)
pponADoc (IntPP n) = texto (show n)
pponADoc (ObjetoPP lista) = (if pponAtomico (snd $ head lista) then entreLlaves2 else entreLlaves) (map aux lista)
                            where entreLlaves2 docs = texto "{ " <+> intercalar (texto ", ") docs <+> texto " }"
                                  aux (str, pp) = texto ((show str) ++ ": ") <+> (pponADoc pp)

