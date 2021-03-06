{-
Name: <Ouanis Seddaoui>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List
import Data.Char

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy _ [] = False
formableBy [] _ = True
formableBy (x:xs) hand = if elem x hand 
                            then formableBy xs (delete x hand)
                         else
                            False    

--
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

--    
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate tem hand str = formableBy str (hand ++ tem) && fits str tem

--
fits :: String -> Template -> Bool
fits [] [] = True
fits _ [] = False
fits [] _ = False
fits (x:xs) ('?':ys) = fits xs ys
fits (x:xs) (y:ys) | x==y = fits xs ys
                   | otherwise = False

--
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate tem hand = filter (wordFitsTemplate tem hand) allWords

--
scrabbleValueWord :: String -> Int
scrabbleValueWord word = foldr ((+) . scrabbleValue ) 0 word 

--
bestWords :: [String] -> [String]
bestWords words = [x | x <- words, scrabbleValueWord x == max]
    where max = maximum[scrabbleValueWord y | y <- words]
             
--
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate = helper 1 0

--
helper :: Int -> Int -> STemplate -> String -> Int
helper a b [] [] = a * b
helper a b (t:ts) (c:cs)
    | t == '?'  = helper a (b + val) ts cs
    | t == 'D'  = helper a (b + val * 2) ts cs
    | t == 'T'  = helper a (b + val * 3) ts cs
    | t == '2'  = helper (a * 2) (b + val) ts cs
    | t == '3'  = helper (a * 3) (b + val) ts cs
    | otherwise = helper a (b + val) ts cs
    where val = scrabbleValue c