-- My first haskell program is a random bridge hand generator. I had to put in lots of extra
-- type declarations to figure out inscrutable type errors messages.

import System.Random
import Data.List

-- We have a 3x3 array of [String] with 4 lines in each.
pad n s = take n (s ++ repeat ' ')
testshowDeal = [[[[c,f,d]|d<-"0123"]|f<-"012"]|c<-"012"]
showDeal dt =  -- dt is 3x3 blocks of 4 lines
  unlines [pad 13 (dt!!blockRow!!0!!lineRow) ++ pad 13 (dt!!blockRow!!1!!lineRow) ++ dt!!blockRow!!2!!lineRow
    | blockRow <- [0,1,2] , lineRow <- [0,1,2,3] ]
nesw = "NESW"
vulcycle = ["Nil","NS","EW","Both","NS","EW","Both","Nil","EW","Both","Nil","NS","Both","Nil","NS","EW"]
cardChar = "AKQJT98765432"
cardPts = [4,3,2,1,0,0,0,0,0,0,0,0,0]::[Int]
cardPoint :: (Int,Int)->Int
cardPoint hcard = cardPts!!(snd hcard)
 
fmtDeal :: Int->[(Int,Int)]->[[[[Char]]]]
fmtDeal n d =
    let
        topLeft::[[Char]]
        topLeft = ["Board "++show n, "Dealer "++[nesw!!((n-1) `mod` 4)], "Vul "++vulcycle!!((n-1) `mod` 16),""]
        hpoints::[[Char]]
        hands::[[[Char]]]
        (hpoints,hands) = unzip [ hpointsSuits | 
            h <- [0,1,2,3],
            let hcards::[(Int,Int)]
                hcards = sort (map (d!!) [h*13 .. h*13+12])
                hpoints :: [Char]
                hpoints = show (foldl (+) 0 (map cardPoint hcards))
                suits :: [[Char]]
                suits = [ suit | s <- [0,1,2,3],
                                 let suit0::[Char]
                                     suit0 = map (cardChar!!) (map snd (filter (\(si,ci)-> si==s) hcards)),
                                 suit <- [if suit0=="" then "-" else suit0] ],
            hpointsSuits <- [(hpoints,suits)]
          ]
        botLeft = ["","  "++hpoints!!0,hpoints!!3++"   "++hpoints!!1,"  "++hpoints!!2]
        blankBlock = ["","","",""]
        --(bl01,bl13,bl21,bl10) = map (fmtHand . (hands!!))  
    in
        [[topLeft,hands!!0,blankBlock],[hands!!3,blankBlock,hands!!1],[botLeft,hands!!2,blankBlock]]

allCards = [(x,y) | x<-[0..3],y<-[0..12]]

getDeal :: RandomGen g => [(Int,Int)] -> g -> ([(Int,Int)],g)
getDeal [] g = ([],g)
getDeal cards g0 = 
    let (n,g1) = randomR (0,length cards - 1) g0
        ch = cards!!n
        (ct,gg) = getDeal (take n cards ++ drop (n+1) cards) g1 
    in
        ((ch:ct),gg) 

getDeals::RandomGen g => g->[[(Int,Int)]]
getDeals g = 
    let (deal,gnext) = getDeal allCards g
    in  deal:(getDeals gnext)
 
main = do
    gen <- getStdGen
    let fmtShow::Int->[(Int,Int)]->[Char]
        fmtShow n deal = showDeal (fmtDeal n deal) ++ "\n"
        deals::[[Char]]
        deals = zipWith fmtShow [1..] (getDeals gen)
    mapM putStr deals
