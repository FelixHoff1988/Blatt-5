{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-
Aufgabe 5_3 a
-}
-- Es werden die Datentypen Point und Vektor erstellt.
data Point = MkPoint {
    px :: Float,
    py :: Float
} deriving (Eq, Ord, Show)

data Vector = MkVector {
    vx :: Float,
    vy :: Float
}deriving (Eq, Ord, Show)
-- Die Funktion addiert einen Vektor mit einem Punkit und gibt einen neuen Punkt aus
--Beispiel translate x1 x2 ergibt (3,3)
translate :: Point -> Vector -> Point
translate (MkPoint p1 p2) (MkVector v1 v2) = MkPoint (p1+v1) (p2+v2)
--Die Funktion multipliziert einen Punkt mit einem Float und skaliert den Punkt dementsprechend.
--Beispiel scale 2 x1 ergibt (4,4) da 2+2 gleich 4 ist
scale :: Float -> Point -> Point
scale factor (MkPoint p1 p2) = MkPoint (factor * p1) (factor * p2)

{-
Aufgabe 5_3 b
-}

class Polygon p where
    area           :: Polygon p => p -> Float
    translate_poly :: Polygon p => p -> Vector -> p
    scale_poly     :: Polygon p => p -> Float -> p

data Triangle = MkTriangle {
    p1 :: Point,
    p2 :: Point,
    p3 :: Point
} deriving Show

data Quad = MkQuad {
    q1 :: Point,
    q2 :: Point,
    q3 :: Point,
    q4 :: Point
} deriving Show

{-
Aufgabe 5_3 c
-}

{-
Funktionen zum Berechnen von Dreiecken
-}

{-
Die Funktion berechnet die Distanz zwischen zwei Punkten und gibt die Länge zurück.
Beispiel lengthBetweenTwoPoints x1 x2 ergibt 6.0
-}
lengthBetweenTwoPoints :: Point -> Point -> Float
lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2) = 
    sqrt((x1-x2)^2 + (y1-y2)^2)

{-
Die Funktion gibt die längste Seite eines allgemeinen Dreiecks zurück.
Beispiel findGround x1 x2 x3 ergibt 6.0.
-}
findGround :: Point -> Point -> Point -> Float
findGround p1 p2 p3 
    | (lengthBetweenTwoPoints p1 p2 > lengthBetweenTwoPoints p1 p3) || (lengthBetweenTwoPoints p1 p2 > lengthBetweenTwoPoints p2 p3) = lengthBetweenTwoPoints p1 p2
    | (lengthBetweenTwoPoints p1 p3 > lengthBetweenTwoPoints p1 p2) || (lengthBetweenTwoPoints p1 p3 > lengthBetweenTwoPoints p2 p3) = lengthBetweenTwoPoints p1 p3
    | otherwise = lengthBetweenTwoPoints p2 p3

{-
Die Funktion berechnet die Summe der Kantenlängen und halbiert diese.
Beispiel calculateHalfOfSumEdge x1 x2 x3 ergibt 7.4966145.
-}
calculateHalfOfSumEdge :: Point -> Point -> Point -> Float
calculateHalfOfSumEdge p1 p2 p3 = 0.5 * (lengthBetweenTwoPoints p1 p2 + lengthBetweenTwoPoints p1 p3 +lengthBetweenTwoPoints p2 p3)

{-
Die Funktion berechnet die Höhe eine allgemeinen Dreiecks aus gehend von der Längsten Seite.
Beispiel calculateHeight x1 x2 x3 ergibt 2.9999995
-}
calculateHeight :: Point -> Point -> Point -> Float
calculateHeight p1 p2 p3 = (2/findGround p1 p2 p3)*sqrt(calculateHalfOfSumEdge p1 p2 p3*(calculateHalfOfSumEdge p1 p2 p3-lengthBetweenTwoPoints p1 p2)*(calculateHalfOfSumEdge p1 p2 p3-lengthBetweenTwoPoints p1 p3)*(calculateHalfOfSumEdge p1 p2 p3-lengthBetweenTwoPoints p2 p3))

{-
Funktionen zum Berechnen von Quadraten
-}

{-
Hilfskonstruktion, weil wir offensichtlich die Aufgabe falsch verstehen.
-}
data Line = MkLine {
    l1 :: Point,
    l2 :: Point
} deriving (Eq, Show)

{-
Die Funktion gibt die beiden Punkte eines Dreiecks zurück, 
die am weitestens auseinander liegen. 
(analog zu findGround nur mit Punkten)
-}
findLongestDistance :: Point -> Point -> Point -> Line
findLongestDistance p1 p2 p3 
    | (lengthBetweenTwoPoints p1 p2 > lengthBetweenTwoPoints p1 p3) || (lengthBetweenTwoPoints p1 p2 > lengthBetweenTwoPoints p2 p3) = MkLine p1 p2
    | (lengthBetweenTwoPoints p1 p3 > lengthBetweenTwoPoints p1 p2) || (lengthBetweenTwoPoints p1 p3 > lengthBetweenTwoPoints p2 p3) = MkLine p1 p3
    | otherwise = MkLine p2 p3

{-
Die Funktion gibt eine beliebige der beiden Diagonalen in einem
allgemeinen Viereck zurück.
-}
findDiagonal :: Quad -> Line
findDiagonal quad = findLongestDistance (q1 quad) (q2 quad) (q3 quad)

{-
Die Funktion gibt die andere Diagonale zurück.
-}
findOtherPoints :: Quad -> Line
findOtherPoints quad 
    |findDiagonal quad == MkLine (q1 quad) (q2 quad) = MkLine(q3 quad) (q4 quad)
    |findDiagonal quad == MkLine (q1 quad) (q3 quad) = MkLine(q2 quad) (q4 quad)
    |findDiagonal quad == MkLine (q2 quad) (q3 quad) = MkLine(q1 quad) (q4 quad)

{-
Die Funktion gibt den 1. Punkt einer Linie zurück.
-}
breakDownLineToFirst :: Line -> Point
breakDownLineToFirst line = l1 line

{-
Die Funktion gibt den 2. Punkt einer Linie zurück.
-}
breakDownLineToSecond :: Line -> Point
breakDownLineToSecond line = l2 line

{-
Wilde Implementierungen der Aufgabenstellungen.
-}
instance Polygon Triangle where
    area (MkTriangle p1 p2 p3) = 0.5* findGround p1 p2 p3 * calculateHeight p1 p2 p3
    translate_poly (MkTriangle p1 p2 p3) v = MkTriangle (translate p1 v) (translate p2 v) (translate p3 v)
    scale_poly (MkTriangle p1 p2 p3) f = MkTriangle (scale f p1) (scale f p2) (scale f p3)
instance Polygon Quad where
    area quad = area (MkTriangle (breakDownLineToFirst (findDiagonal quad)) (breakDownLineToSecond (findDiagonal quad))(breakDownLineToFirst (findOtherPoints quad))) + area (MkTriangle (breakDownLineToFirst (findDiagonal quad)) (breakDownLineToSecond (findDiagonal quad))(breakDownLineToSecond (findOtherPoints quad)))
    translate_poly quad v = MkQuad (translate (q1 quad) v) (translate (q2 quad) v) (translate (q3 quad) v) (translate (q3 quad) v)
    scale_poly quad f = MkQuad (scale f (q1 quad)) (scale f (q2 quad)) (scale f (q3 quad)) (scale f (q4 quad))

{-
Konstanten zum Testen der Funktionen
-}
x1 :: Point
x1 = MkPoint 2 2

v1:: Vector
v1 = MkVector 1 1

x2 :: Point
x2 = MkPoint 8 2

x3 :: Point
x3 = MkPoint 7 5

x4 :: Point
x4 = MkPoint 3 1

t1 :: Triangle
t1 = MkTriangle x1 x2 x3

t2 :: Triangle
t2 = MkTriangle x1 x2 x4

q :: Quad
q = MkQuad x1 x2 x3 x4

line1 :: Line
line1 = MkLine x1 x2
