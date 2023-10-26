-- Some functions in Haskell

-- Vektoren addieren
addVectors::(Num a) => Vector a -> Vector a -> Vector a  
addVectors (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

-- Skalarprodukt
scalarProduct::(Num a) => Vector a -> Vector a -> a  
scalarProduct (x1,y1,z1) (x2,y2,z2) = x1*x2+y1*y2+z1*z2

-- Rechtwinkligkeit
scalarProduct::(Eq a, Num a) => Vector a -> Vector a -> a  
scalarProduct (x1,y1,z1) (x2,y2,z2) = x1*x2+y1*y2+z1*z2  

pairwiseAngled::(Eq a, Num a) => [Vector a] -> [Vector a] -> [(Vector a, Vector a)]  
pairwiseAngled a b = [(x,y)|x<-a, y<-b, scalarProduct x y == 0]

-- Mengen
rm:: (Eq a) => [a] -> [a]
rm [] = []
rm (x:xs)
	| elem x xs = rm xs
	| otherwise = x:  rm xs

-- Bedingtes Summieren
sumOddNumbersSmallerThanN:: (Ord a, Integral a) => a -> [a] -> a  
sumOddNumbersSmallerThanN n [] = 0  
sumOddNumbersSmallerThanN n (x:xs)  
    | x < n && rem x 2 /= 0 = x + sumOddNumbersSmallerThanN n xs  
    | otherwise = sumOddNumbersSmallerThanN n xs

-- Liste gerader Zahlen
dropOddNumbers:: (Ord a, Integral a) => [a] -> [a]  
dropOddNumbers [] = []  
dropOddNumbers (x:xs)  
    | rem x 2 == 0 = x : dropOddNumbers xs  
    | otherwise = dropOddNumbers xs

-- Paarweise Summieren
pairwiseSum::(Num a) => [a] -> [a] -> [a]  
pairwiseSum [] []                = []
pairwiseSum [] yss@(y:ys)  = yss 
pairwiseSum xss@(x:xs) []  = xss
pairwiseSum (x:xs) (y:ys)    = (x + y) : pairwiseSum xs ys

-- Bedingtes Rauswerfen
dropWhileXGreaterThanThree::(Ord a, Num a) => [a] -> [a]  
dropWhileXGreaterThanThree [] = []  
dropWhileXGreaterThanThree (x:xs)  
    | x > 3 = dropWhileXGreaterThanThree xs  
    | otherwise = x : xs

-- oder einfach
dropWhile (x>3) [a]

-- Voraussetzung: keine
-- Ergebnis: Anzahl der Elemente in einer Liste, die das Prädikat nicht erfüllen.
countIfNot :: (Eq a) => (a -> Bool) -> [a] -> Int  
countIfNot b l = length (filter (not . b) l)

-- Voraussetzung: keine
-- Ergebnis: Das Element, das am häufigsten nacheinander in der Liste steht.
maxConReps :: (Eq a) => [a] -> a
maxConReps [] = eroor "Empty list"
maxConReps (x:xs) = help (x, 1) (x, 0) xs  
    where  
    help acc _ [] = fst acc  
    help acc (_, 0) (x:xs)  
        | fst acc == x  = help (x, snd acc + 1) (x, 0) xs  
        | otherwise     = help acc (x,1) xs  
    help acc cm (x:xs)  
        | snd acc < snd cm = help cm (x,0) (x:xs)  
        | fst cm == x           = help acc (x, snd cm + 1) xs  
        | otherwise              = help acc (x, 1) xs

-- Voraussetzung: keine
-- Ergebnis: n Primzahlen, die auf 3 enden, als eine Liste.
primeThree :: Int -> [Int]  
primeThree n = take n (filter ((=='3') . last . show) primes)

-- Voraussetzung: keine
-- Ergebnis: Liste von Elementen, auf die die Funktion angewandt war, solange die Elemente das Prädikat erfüllen.
mapUntil :: (Eq a) => (a -> a) -> (a -> Bool) -> [a] -> [a]  
mapUntil f p list = takeWhile p (map f list)

-- Voraussetzung: keine
-- Ergebnis: loklen Minima einer Liste.
localMin :: (Eq a, Ord a) => [a] -> [a]  
localMin []          = []  
localMin [x]        = [x]  
localMin (x:y:xs) = if (x < y) then (x : (cmpr x y xs)) else (cmpr x y xs)  
    where  
    cmpr x y []  
        | y < x         = [y]  
        | otherwise = []  
    cmpr x y xs  
        | (y < x) && (y < (head xs)) = y : (cmpr y (head xs) (tail xs))  
        | otherwise                          = cmpr y (head xs) (tail xs)

-- Code für eine größere Aufgabe

data FTree a = Leaf a | FNode a (FTree a) (FTree a)
data BTree a = Nil | BNode a (BTree a) (BTree a)

-- Voraussetzung: keine
-- Ergebnis: Liste von Listen, die in FTree waren, auf den Funktion f angewandt war.
mapListTree :: FTree [a] -> (a -> b) -> [[b]]  
mapListTree (Leaf a) f = [map f a]  
mapListTree (FNode a lt rt) f = [map f a] ++ mapListTree lt f ++ mapListTree rt f

-- Voraussetzung: keine
-- Ergebnis: True, wenn FTree balanciert ist, sonst False.
balanced :: FTree a -> Bool  
balanced (Leaf _) = True  
balanced (FNode _ (Leaf _) (FNode _ (FNode _ _ _) (FNode _ _ _))) = False  
balanced (FNode _ (Leaf _) (FNode _ (Leaf _) (FNode _ _ _)))          = False  
balanced (FNode _ (Leaf _) (FNode _ (FNode _ _ _) (Leaf _)))          = False  
balanced (FNode _ (FNode _ (FNode _ _ _) (FNode _ _ _)) (Leaf _)) = False  
balanced (FNode _ (FNode _ (Leaf _) (FNode _ _ _)) (Leaf _))          = False  
balanced (FNode _ (FNode _ (FNode _ _ _) (Leaf _)) (Leaf _))          = False  
balanced (FNode _ lt rt)                                              = (balanced lt) && (balanced rt)

-- Voraussetzung: keine
-- Ergebnis: Ftree zur Btree umwandeln
convert :: FTree a -> BTree a  
convert (Leaf a) = (BNode a Nil Nil)  
convert (FNode d lt rt) = (BNode d (convert lt) (convert rt))

-- Ende von code für eine größere Aufgabe

data Directions = L | R deriving (Eq, Ord, Show)
type Code = [Directions]
type CodeTable = [(Char, Code)]

-- Voraussetzung: Keine Kodierung kommt doppelt vor.
-- Ergebnis: True, wenn Code ein Präfixcode ist, sonst False
isPrefix :: CodeTable -> Bool  
isPrefix table = combinator (map snd table)  
    where  
    combinator :: [Code] -> Bool  
    combinator []         = True  
    combinator [x]       = True  
    combinator (x:y:xs) = (checker x y) && (combinator (x:xs)) && (combinator (y:xs)) && (combinator xs)  
  
    checker :: Code -> Code -> Bool  
    checker [] _ = False  
    checker _ [] = False  
    checker (x:xs) (y:ys)  
        | x == y = checker xs ys  
        | otherwise = True

-- Voraussetzungen: t und v sind Zahlen. t ist die Temperatur und v ist die Windgeschwindigkeit.  
-- Ergebnis: gefühlte Temperatur. Ergebnis ist auch eine Zahl.  
windchill:: (Floating a) => a -> a -> a  
windchill t v = 13.12 + 0.6215 * t + (0.3965 * t - 11.37) * (v**0.16)

-- Voraussetzung: Als Eingabe gelten nur Zeichen und Zeichenketten.  
-- Ergebnis: True, wenn eingegebenes Zeichen eine Ziffer ist und False, wenn nicht.  
isanum_a:: Char -> Bool  
isanum_a x = x=='1' || x=='2' || x=='3' || x=='4' || x=='5' || x=='6' || x=='7' || x=='8' || x=='9' || x=='0'

-- Voraussetzung: Als Eingabe gelten nur Zeichen und Zeichenketten.  
-- Ergebnis: Für alle Ziffern wird ein Satz mit diese Ziffer ausgegeben, für nicht Ziffern wird ein Satz ausgegeben, dass die Eingabe keine Ziffer ist.  
isanum_b:: Char -> String  
isanum_b '0' = "Die Ziffer ist 0."  
isanum_b '1' = "Die Ziffer ist 1."  
isanum_b '2' = "Die Ziffer ist 2."  
isanum_b '3' = "Die Ziffer ist 3."  
isanum_b '4' = "Die Ziffer ist 4."  
isanum_b '5' = "Die Ziffer ist 5."  
isanum_b '6' = "Die Ziffer ist 6."  
isanum_b '7' = "Die Ziffer ist 7."  
isanum_b '8' = "Die Ziffer ist 8."  
isanum_b '9' = "Die Ziffer ist 9."  
isanum_b _ = "Es ist keine Ziffer."

--Voraussetzung: Als Eingabe gelten nur Zeichen und Zeichenketten.  
--Ergebnis: True, wenn eingegebenes Zeichen eine Ziffer ist und False, wenn nicht.  
isanum_c:: Char -> Bool  
isanum_c x  
    | x == '0'      = True  
    | x == '1'      = True  
    | x == '2'      = True  
    | x == '3'      = True  
    | x == '4'      = True  
    | x == '5'      = True  
    | x == '6'      = True  
    | x == '7'      = True  
    | x == '8'      = True  
    | x == '9'      = True  
    | otherwise     = False

--Voraussetzung: Jahr wird als eine ganze Zahl angegeben.  
--Ergebnis: Es wird eine Verbale Beschreibung ausgegeben, ob ein Jahr ein Schaltjahr ist oder nicht.  
schaltjahr:: (Integral a) => a -> [Char]  
schaltjahr x  
    | ((rem x 4) == 0) && (not(rem x 100 == 0) || (rem x 400 == 0)) = "Das angegebene Jahr ist ein Schaltjahr."  
    | otherwise = "Das angegebene Jahr ist kein Schaltjahr."

zinsen :: Double -> Double -> Double  
zinsen kapital zinsfuss = kapital * zinsfuss * 0.01

--Voraussetzung: Man muss den Anfangskapital und Zinsfuss als Zahlen übergeben.  
--Ergebnis: Es wird der Wert der Anlage am Ende einer Zinsperiode berechnet.  
endwert:: Double -> Double -> Double  
endwert kapital zinsfuss = kapital + (zinsen kapital zinsfuss)

--Voraussetzung: Man muss den Anfangskapital und Zinsfuss als Zahlen übergeben.  
--Ergebnis: Es wird der Wert der Anlage am Ende von zwei Zinsperioden berechnet.  
endwert2:: Double -> Double -> Double  
endwert2 kapital zinsfuss = (endwert kapital zinsfuss) + (zinsen (endwert kapital zinsfuss) zinsfuss)  
-- [k1 + (k1 * z * 0.01)] + (k2 * z * 0.01)

--Voraussetzung: Als Eingabe gilt eine Liste von ganzen Zahlen.  
--Ergebnis: Die Summe von Zahlen, die durch 3 oder 4 teilbar sind, aber nicht durch beides, als eine ganze Zahl.  
rek_sum:: (Eq a, Integral a) => [a] -> a  
rek_sum [] = 0  
rek_sum (x:xs)  
    | (rem x 3 == 0) && (rem x 4 == 0) = rek_sum xs  
    | (rem x 3 == 0) || (rem x 4 == 0) = x + rek_sum xs  
    | otherwise = rek_sum xs

-- a)  
--Voraussetzung: Als Eingabe gilt eine Liste von ganzen Zahlen  
--Ergebnis: Die Anzahl von Einsen in der Liste als eine ganze Zahl.  
countOnes:: (Eq a, Integral a) => [a] -> a  
countOnes [] = 0  
countOnes (x:xs)  
    | x == 1 = x + countOnes xs  
    | otherwise = countOnes xs  
  
  
-- b)  
--Voraussetzung: Als Eingabe gilt eine Liste von ganzen Zahlen  
--Ergebnis: Anzahl der Vorkommen von einer als Parameter eingegebener Zahl.  
count:: (Eq a, Integral a) => [a] -> a -> a  
count [] n = 0  
count (x:xs) n  
    | x == n = 1 + count xs n  
    | otherwise = count xs n

-- Zählen wie ein Troll
-- a) Zahl 18 = LOTS-Two. Zahl 33 = LOTS-LOTS-1.  
  
-- b)  
--Voraussetzung: Als Eingabe gilt eine ganze Zahl >= 0.  
--Ergebnis: Verbale Beschreibung, wie ein Troll die eingegebene Zahl sagen würde.  
countAsTroll:: (Eq a, Integral a) => a -> String  
countAsTroll n  
    | n == 0 = ""++"Zero"  
    | n == 1 = ""++"One"  
    | n == 2 = ""++"Two"  
    | n == 3 = ""++"Three"  
    | n == 4 = ""++"Many"  
    | n < 16 =  ""++"Many-"++countAsTroll (n-4)  
    | n == 16 = ""++"LOTS"  
    | n > 16 = ""++"LOTS-"++countAsTroll (n-16)

--Voraussetzung: Als Eingabe gilt eine Liste von Elementen, die man paarweise vergleichen kann.  
--Ergebnis: True, wenn die Liste aufsteigend sortiert ist und False, wenn nicht.  
isu :: (Ord a) => [a] -> Bool  
isu [] = True  
isu [x] = True  
isu (x:y:xs)
| x <= y = isu (y:xs)  
| otherwise = False

-- Bubblesort
--Voraussetzung: Als Eingabe gilt eine Liste.  
--Ergebnis: Sortierte Liste.  
  
bbHelp :: (Ord a) => Int -> [a] -> [a]  
bbHelp 0 lst = lst  
bbHelp i lst = bbHelp (i-1) (swp lst)  

swp :: (Ord a) => [a] -> [a]  
swp [] = []  
swp [x] = [x]  
swp (x:y:xs)  
| x < y = x:(swp (y:xs))  
| otherwise = y:(swp (x:xs))  
  
bbSort :: (Ord a) => [a] -> [a]  
bbSort lst = bbHelp (length lst) lst

-- Rekursion und Endrekursion
-- (a)  
-- Voraussetzung: Als Eingabe gilt eine ganze Zahl.  
-- Ergebnis: Die Zahl umgedreht. Negative Zahlen werden positiv ausgegeben.  
numrev:: Integer -> Integer  
numrev = read . reverse . show . abs  
  
  
-- (b) Ich nehme für Lösung der Aufgabe an, dass read im Haskell endrekursiv implementiert ist.  
-- Voraussetzung: Als Eingabe gilt eine ganze Zahl.  
-- Ergebnis: Die Zahl umgedreht. Negative Zahlen werden positiv ausgegeben.  
numrev2:: Integer -> Integer  
numrev2 num = read(revv [] (show . abs $ num))  
where  
revv:: [a] -> [a] -> [a]  
revv acc [] = acc  
revv acc (x:xs) = revv (x:acc) xs  
  
-- Die Funktion ist endrekursiv, da zuerst alle Rekursionsschritte ausgeführt werden (intern und im read)  
-- und nur dann das Endwert berechnet.  
  
  
-- (c)  
-- Voraussetzung: Als Eingabe gilt eine ganze Zahl.  
-- Ergebnis: Die Zahl umgedreht. Negative Zahlen werden positiv ausgegeben.  
schorsch:: Integer -> Integer  
schorsch num = read(foldl (flip (:)) [] (show . abs $ num))  
  
-- schorsch ist intern endrekursiv, da hier (foldl) genutzt wird. (foldl) ist selbst  
-- endrekursiv.  
  
-- (d) Ich nehme für Lösung der Aufgabe an, dass read im Haskell endrekursiv implementiert ist.  
-- Voraussetzung: Als Eingabe gilt eine ganze Zahl.  
-- Ergebnis: Die Zahl umgedreht. Negative Zahlen werden positiv ausgegeben.  
numrev4:: Integer -> Integer  
numrev4 num = read(revv [] (show . abs $ num))  
where  
revv:: [a] -> [a] -> [a]  
revv acc [] = acc  
revv acc (x:xs) = revv ((x:) $! acc) xs

-- Map und Faltung
--(a)  
-- Voraussetzung: Es muss eine Funktion und eine Liste von Zahlen eingegeben werden.  
-- Ergebnis: Parameter, für den die Funktion das maximale Wert hat.  
argmax:: (Ord a) => (a -> a) -> [a] -> a
argmax f lst = if [] then (error "Empty list") else (snd (foldr (max) (f (head lst), (head lst)) (zip (map f (tail lst)) (tail lst))))  
  
--(b)  
-- Voraussetzung: Als Eingabe gilt ein Parameter als eine Zahl und eine Liste von ganzen, nicht negativen Zahlen.  
-- Ergebnis: Die Quersummen von Zahlen, die größer als der Parameter sind.  
checksumLargerThan:: Integer -> [Integer] -> [Integer]  
checksumLargerThan k lst = filter (k<) (map (quersum) lst)  
  
-- Voraussetzung: Als eingabe gilt eine ganze, nicht negative Zahl.  
-- Ergebnis: Die Quersumme von der Zahl.  
quersum:: Integer -> Integer  
quersum num = foldr (+) 0 (map (read . (:[])) . show $ num)  
  
  
--(c)  
-- Voraussetzung: Keine.  
-- Ergebnis: Eine Liste, wobei die Funktion auf alle Elemente der Liste angewandt wurde.  
mapByFold:: (a -> b) -> [a] -> [b]  
mapByFold f lst = foldr (\ x acc -> f x : acc ) [] lst

-- scanl und scanr
{-  
Quellen: http://zvon.org/other/haskell/Outputprelude/scanr_f.html  
http://zvon.org/other/haskell/Outputprelude/scanl_f.html  
  
(scanr) (a -> b -> b) -> b -> [a] -> [b]. (scnar) funktioniert ähnlich zu (foldr),  
aber gibt die Liste zurück, die Endwerte als auch Zwischenwerte enthält. (scanr) nimmt  
den zweiten Parameter und den letzten Element der Liste , wendet die Funktion an usw.  
  
(scanl) (a -> b -> a) -> a -> [b] -> [a]. (scanl) funktioniert ähnlich zu (foldl),  
aber gibt die Liste mit Endwerten und Zwischenwerten zurück. (scanl) nimmt dern zweiten  
Parameter und der ersten Element der Liste, wendet die Funktion an usw.  
-}  
  
-- Implementierung scanr
scanr :: (a -> b -> b) -> b -> [a] -> [b]  
scanr _ arg [] = [arg]  
scanr f arg (x:xs) = f x q : qs  
	where qs@(q:_) = _scanr f arg xs

-- Implementierung scanl
scanl :: (a -> b -> a) -> a -> [b] -> [a]  
scanl f arg [] = [arg]  
scanl f arg (x:xs) = arg : (_scanl f (f arg x) xs)
  
-- Zu scanr.  
-- Voraussetzung: Als Eingabe gilt eine Liste von booleschen Werten.  
-- Ergebnis: Eine Liste, die den Verlauf von alle Vergleiche zeigt.  
ex1:: [Bool] -> [Bool]  
ex1 lst = scanr (==) True lst  
  
-- Zu scanl.  
-- Voraussetzung: Als Eingabe gilt eine Liste von Zahlen.  
-- Ergebnis: Eine Liste, in der shrittweise die Ergebnisse von Vergleich der Zahlen in der Liste  
-- gezeigt werden.  
ex2:: [Integer] -> [Integer]  
ex2 nums = scanl (\ x acc -> max x acc) (head nums) (tail nums)

-- Gringotts (Typsynonyme und Datentypen)
--(a)  
  
type Knuts = Integer  
type Sickel = Integer  
type Galleonen = Integer  
  
data ZaubererGeld = Preis Galleonen Sickel Knuts deriving (Show, Eq, Ord)  
  
  
--(b)  
  
-- Voraussetung: Eingabe in Form (Preis num_1 num_2 num_3). Erste Eingabe ist der Betrag zum Abheben, zweite ist Verlies.  
-- Ergebnis: Rest in einem Verlies, nachdem die Abhebung erfolgte.  
differenz :: ZaubererGeld -> ZaubererGeld -> ZaubererGeld  
differenz (Preis g1 s1 k1) (Preis g2 s2 k2) = diff (convert (Preis g1 s1 k1)) (convert (Preis g2 s2 k2))  
  
-- Voraussetung: Eingabe in Form (Preis num_1 num_2 num_3).  
-- Ergebnis: Knuts werden zu Sickel und Sickel zu Galleonen konvertiert, wenn möglich.  
convert :: ZaubererGeld -> ZaubererGeld  
convert (Preis g s k)  
| k >= 29 = convert (Preis g (s + (div k 29)) (rem k 29))  
| s >= 17 = convert (Preis (g + (div s 17)) (rem s 17) k)  
| otherwise = (Preis g s k)  
  
-- Voraussetung:  
-- Ergebnis:  
diff :: ZaubererGeld -> ZaubererGeld -> ZaubererGeld  
diff (Preis g1 s1 k1) (Preis g2 s2 k2) = (Preis (g2-g1) (s2-s1) (k2-k1))  
  
  
{-  
Mr. Weasly kann 2 Galleonen abheben und es bleiben 4 Knuts in seinem Verlies.  
  
Es ist sinnvoll, die kleinste Einheit in die größte zu konvertieren und dann die Differenz zu bilden  
(Galleonen <- Sickel <- Knuts statt Galleonen -> Sickel -> Knuts), weil man die Differenz auch sinnvoll  
darstellen soll. So vermeidet man den Schritt, wo man noch 60000 Knuts konvertieren muss, um die sinvoll  
darzustellen.  
  
convert "normalisiert" den Betrag, den die Kunde abheben möchte und die Menge von vorhandenen Geld im  
Verlies. Wenn diese Funktion vorhanden ist, dann ist es egal, auf welche Weise die Kunde den Betrag eingibt,  
da es automatisch konvertiert wird.  
  
diff ermöglicht es, eine Fallunterscheidung und Anwendung von convert zu kombinieren. Man kann alle Operationen  
(diff und convert) in eine Zeile schreiben, was aber die Lesbarkeit verringert.  
-}  
  
--(c)  
  
-- Voraussetung: Eingabe in Form (Preis num_1 num_2 num_3). Erste Eingabe ist der Betrag zum Abheben, zweite ist Verlies.  
-- Ergebnis: True, wenn es ausreichend Geld im Verlies ist und False, wenn nicht.  
kannAbheben :: ZaubererGeld -> ZaubererGeld -> Bool  
kannAbheben (Preis g1 s1 k1) (Preis g2 s2 k2) = help (differenz (Preis g1 s1 k1) (Preis g2 s2 k2))  
where  
help (Preis x y z)  
| x < 0 || y < 0 || z < 0 = False  
| otherwise = True

-- Rekursive Datentypen und Typklassen
--(a)  
  
data Nat = Zero | S Nat deriving Show  
  
zero = Zero  
one = S(Zero)  
two = S(S(Zero))  
three = S(S(S(Zero)))  
four = S(S(S(S(Zero))))  
five = S(S(S(S(S(Zero)))))  
six = S(S(S(S(S(S(Zero))))))  
seven = S(S(S(S(S(S(S(Zero)))))))  
eight = S(S(S(S(S(S(S(S(Zero))))))))  
nine = S(S(S(S(S(S(S(S(S(Zero)))))))))  
  
--(b)  
  
-- Voraussetung: Eingabe in der Form (S Nat).  
-- Ergebnis: Die eingegebene natürliche Zahl bestehend aus Ziffern 0-9.  
_show :: Nat -> String  
_show Zero = "0"  
_show (S n) = help (S n) 0  
where  
help Zero acc = show(acc)  
help (S n) acc = help n (acc+1)  
  
--(c)  
  
addNat :: Nat -> Nat -> Nat  
addNat Zero b = b  
addNat (S a) b = S (addNat a b)  
  
multNat :: Nat -> Nat -> Nat  
multNat Zero b = Zero  
multNat (S a) b = addNat (multNat a b) b  
  
  
-- Voraussetung: Zwei natürliche Zahlen in der Form (S Nat) als Eingabe. Die erste Zahl entspricht a und die zweite Zahl entspricht b im (a ^ b)  
-- Ergebnis: Die Potenz von beiden Zahlen.  
powerNum :: Nat -> Nat -> Nat  
powerNum _ Zero = (S(Zero))  
powerNum (S a) (S(Zero)) = (S a)  
powerNum (S a) (S b) = multNat (S a) (powerNum (S a) b)
