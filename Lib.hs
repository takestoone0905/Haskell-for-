module Lib
    where
        import Data.List


        quickSort [] = []
        quickSort [x] =[x]
        quickSort (x:xs)=(quickSort smaller)++[x]++(quickSort larger)
            where
                smaller=[s|s<-xs,s<x]
                larger=[s|s<-xs,s>=x]
        
        quickSortRow::Ord a =>[[a]]->[[a]]
        quickSortRow (xss) = [quickSort xs|xs<-xss]

        warshallFloyd xxs = [bellmanFord i xxs|i<-[0..(length xxs)-1]]

        bellmanFord from xxs =[bellmanFordHelper from i xxs|i<-[0..(length xxs)-1]]
        bellmanFordHelper from to xxs 
            |from == to = 0
            |otherwise= minimum [min (xxs!!from!!to) ((xxs!!from!!i)+(xxs!!i!!to))|i<-[0..(length xxs)-1],i/=from,i/=to]

        getGCD::Integer->Integer->Integer
        getGCD m n 
            | m < n = (getGCD n m)
            | n == 0 = m
            | otherwise = getGCD (mod m n) n

        getLCM::Integer->Integer->Integer
        getLCM m n = div (m * n) (getGCD m n)
