module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 5" ~: testsEj6,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),
      mostrar (linea <+> linea) ~?= "\n\n",
      (vacio <+> texto "a") ~?= texto "a",
      (texto "a" <+> vacio) ~?= texto "a",
      linea <+> vacio ~?= linea,
      vacio <+> linea ~?= linea
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      indentar 0 (linea <+> texto "a") ~?= linea <+> texto "a",
      indentar 0 vacio ~?= vacio
      
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      mostrar (texto "hola") ~?= "hola",
      mostrar (texto "hola" <+> linea <+> texto "mundo") ~?= "hola\nmundo"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]
pedro = ObjetoPP [("p", pericles), ("m", merlina), ("a", addams)]
hola = TextoPP "hola"
numero = IntPP 9

testsEj5 :: Test
testsEj5 = 
    test 
      [ pponAtomico (TextoPP "hola") ~?= True,
        pponAtomico (IntPP 8) ~?= True,
        pponAtomico (pedro) ~?= False,
        pponAtomico (ObjetoPP [("1", TextoPP "a")]) ~?= False,
        pponAtomico (ObjetoPP [("a", addams)]) ~?= False,
        pponAtomico (ObjetoPP []) ~?= False
      ]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple merlina ~?= True,
      pponObjetoSimple (ObjetoPP []) ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple pedro ~?= False
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"
ola = texto "ola"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      mostrar (intercalar linea [texto "a", texto "b", texto "c"]) ~?= "a\nb\nc",
      mostrar (entreLlaves [intercalar (texto ", ") [a, b, c]]) ~?= "{\n  a, b, c\n}",
      mostrar (intercalar (texto ", ") [(entreLlaves [intercalar (texto ", ") [a]]),(entreLlaves [intercalar (texto ", ") [b]])]) ~?= "{\n  a\n}, {\n  b\n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      mostrar (aplanar ola <+> a) ~?= "olaa",
      mostrar (aplanar a) ~?= "a",
      mostrar (aplanar vacio) ~?= "",
      mostrar (aplanar (a <+> linea <+> linea <+> c)) ~?= "a  c"
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      mostrar (pponADoc hola) ~?= "\"hola\"",
      mostrar (pponADoc numero) ~?= "9",
      mostrar (pponADoc (ObjetoPP [])) ~?= "{ }",

      mostrar (pponADoc (ObjetoPP [
        ("texto", TextoPP "valor"),
        ("numero", IntPP 42),
        ("objeto", ObjetoPP [
          ("subtexto", TextoPP "subvalor"),
          ("subnumero", IntPP 24)
        ])
      ])) ~?= "{\n  \"texto\": \"valor\",\n  \"numero\": 42,\n  \"objeto\": { \"subtexto\": \"subvalor\", \"subnumero\": 24 }\n}"
      
    ]
