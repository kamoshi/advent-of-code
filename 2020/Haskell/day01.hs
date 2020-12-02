main = do  
        contents <- readFile "input.txt"
        let list = map (read :: String -> Int) (words contents)
        let p1 = head [((a, b), a*b) | a <- list, b <- list, a+b == 2020]
        let p2 = head [((a,b,c),a*b*c) | a <- list, b <- list, c <- list, a+b+c == 2020]
        print(p1)
        print(p2)
