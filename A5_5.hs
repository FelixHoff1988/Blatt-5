{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
-- Erstellt wird der Datentyp Currency der aus  coins (Integer), cents(Integer) und currency (Sting) besteht.
data Currency = MkCurrency {
    coins :: Integer,
    cents :: Integer,
    currency :: String
}
-- Funktion nimmt 3 Elemente entgegen und gibt coins aus.
-- Beispiel getCoins myEuro gibt 12 aus da er nur die ganzen Euros ausgibt
getCoins :: Currency -> Integer
getCoins (MkCurrency coins cents currency) = coins
-- Funktion nimmt 3 Elemente entgegen und gibt cents aus.
-- Beispiel getCoins myEuro gibt 03 aus da er nur die Cents ausgibt
getCents :: Currency -> Integer
getCents (MkCurrency coins cents currency) = cents
-- Funktion nimmt 3 Elemente entgegen und gibt currency aus.
-- Beispiel getCoins myEuro gibt Euro aus da er die aktuelle Währung aisgibt.
getCurrency :: Currency -> String
getCurrency (MkCurrency coins cents currency) = currency
-- Festlegung für die Anzeige der Geldbeträge mit einem Komma in Euro und Dollar (bzw. Yen wenn keiner dieser )
instance Show Currency where
    show(MkCurrency coins cents currency) = 
        if currency == "Euro" || currency == "Dollar" 
        then show coins ++ "," ++ show cents ++ " " ++ currency
        else show coins ++ " " ++ currency

-- Die Funktionen erstellen Beispiele für jeweils Euro, Dollar und Yen aus dem Datentyp Currency 
myEuro :: Currency
myEuro = MkCurrency 12 03 "Euro"

myDollar :: Currency
myDollar = MkCurrency 14 60 "Dollar"

myYen :: Currency
myYen = MkCurrency 120 0 "Yen"

-- Die Funktion nimmt eine Currency die entweder aus einem oder zwei Integer besteht und wandelt diese in ein Float um
-- Beispiel currencyToFloat myEuro sollte 12.03 ausgeben
-- Beispiel currencyToFloat myDollar sollte 14.60 ausgeben
-- Beispiel currencyToFloat myYen sollte 120.0 ausgeben
currencyToFloat :: Currency -> Float
currencyToFloat currency = fromIntegral (getCoins currency) + fromIntegral (getCents currency)/(10^2)

-- Die Funktion nimmt eine Currency und einen beliebigen Kurs und wandelt die Currency dementsprechend um.
--Beispiel exchangeRate myEuro 2 sollte 6.015 ausgeben da dieser durch 2 geteilt wird.
--Beispiel exchangeRate myDollar 2 sollte 7.3 ausgeben da dieser durch 2 geteilt wird.
exchangeRate :: Currency -> Float -> Float
exchangeRate currency rate = if rate >= 1.0 then currencyToFloat currency/rate else currencyToFloat currency*rate

-- Die Funktion wandelt eine Währung in eine andere um. Es muss die aktuelle Währung eingegeben werden, die exchange rate und in welche Währung es umgewandelt werden soll. Als Ergebnis wird die neue Währung als Currency ausgegeben.
--Beispiel exchangeCurrency myDollar 2 "Euro" gibt 7.30 Euro aus. Da in diesem Beispiel die exchange rate gleich 2 ist.
exchangeCurrency :: Currency -> Float -> String -> Currency
exchangeCurrency currency rate newcurrency = MkCurrency (truncate  (exchangeRate currency rate)) (floatToDecimalPlace(exchangeRate currency rate)) newcurrency 

-- Die Funktion gibt die letzten beiden Nachkommastellen eines Float als Integers zurück
-- Beispiel: floatToDecimalPlace 12.45 sollte 45 ergeben 
floatToDecimalPlace :: Float -> Integer
floatToDecimalPlace f = round ((f-fromIntegral (truncate f))*10^2)

-- Die Funktion toEuro nimmt eine Currency rechnet sie in Euro um und gibt den Euro als Currency Wert ab.
--Beispiele toEuro myDollar sollte 13,14 Euro ausgeben, da 14.60 / 0.9 = 13.14 ergibt
--Beispiele toEuro myYen sollte 0.99 Euro ausgeben, da 120 * 0.0083 = 13.14 ergibt
toEuro :: Currency -> Currency
toEuro currency
    | getCurrency currency == "Dollar" = exchangeCurrency currency 0.9 "Euro"
    | getCurrency currency == "Yen" = exchangeCurrency currency 0.0083 "Euro"
    | otherwise = currency
    
