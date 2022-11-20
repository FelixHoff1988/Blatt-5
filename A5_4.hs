--Erstellt wird der Datentyp Rank der aus den Zahlen von 7 bis Ass eines Kartenspiels besteht.
data Rank = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
--Erstellt wird der Datentyp Suit der aus den Farben/Typen eines Kartenspiels besteht. Also Herz,Karo,Piek,Kreuz.
data Suit = Diamond | Heart | Spade | Club deriving (Show, Eq, Ord)
--Erstellt wird der Datentyp Card der jeweils aus einer Zahl und einer Farbe/Typ besteht.
data Card = MkCard {
    rank :: Rank,
    suit :: Suit
    } 
    deriving (Show, Eq)
-- Card wird als Instanz der Klasse Ord erstellt. Nun können 2 Karten verglichen werden welche einen höheren Wert hat. Der Wert Rank entscheidet welche Karte größer ist, bei zwei Karten gleichen Wertes endscheidet die Farbe/Typ welche Karte größer ist. Bei gleichem Wert und gleicher Farbe sind sie gleich groß-
--Beispiel card1 <= card2 ergibt False da King größer als die 9 ist
--Beispielt card3 <= card2 ergibt True da die 7 kleiner als die 9 ist.
instance Ord Card where
    (MkCard rank1 suit1) <= (MkCard rank2 suit2) = if rank1 == rank2 then suit1 <= suit2 else rank1 <= rank2

-- Es wird der Datentyp Hand implementiert, eine Hand besteht aus 3 Karten.
data Hand = MkHand Card Card Card
--Die Funktion Value nimm eine Hand und gibt ihr einen Wert von 0 bis 3. Falls Die Farben/Typ der 3 Karten gleich ist erhält sie den Value Wert 3. Falls der Rank der drei Karten gleich ist, sie also ein Drilling sind erhält die Hand den Value Wert 2. Falls zwei von den drei Karten denselben Rank haben also ein Paar vorhanden ist, erhält sie den Value Wert 1. Falls nichts von dem oben genannten der Fall ist erhält sie den Value Wert 0.
--Beispiel value myHand ergibt 0, da Die Farben der drei Karten nicht gleich sind und weder ein Drilling noch ein Paar vorhanden ist.
--Beispiel value myHand1 ergibt 3, da alle 3 Farben der drei Karten gleich sind
value :: Hand -> Integer
value (MkHand (MkCard rank1 suit1) (MkCard rank2 suit2) (MkCard rank3 suit3)) 
    | suit1 == suit2 && suit1 == suit3 && suit2 == suit3 = 3
    | rank1 == rank2 && rank1 == rank3 && rank2 == rank3 = 2
    | rank1 == rank2 || rank1 == rank3 || rank2 == rank3 = 1
    | otherwise = 0
-- Erstellt wird eine Hand die aus drei Karten besteht
myHand :: Hand
myHand = MkHand card1 card2 card3

card1 :: Card
card1 = MkCard King Club

card2 :: Card
card2 = MkCard Nine Heart

card3 :: Card
card3 = MkCard Seven Heart
-- Erstellt wird eine Hand die aus drei Karten besteht
myHand1 :: Hand
myHand1 = MkHand aceclub kingclub queenclub

aceclub :: Card
aceclub = MkCard Ace Club

kingclub :: Card
kingclub = MkCard King Club

queenclub :: Card
queenclub = MkCard Queen Club
