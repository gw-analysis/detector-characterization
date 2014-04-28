
doubleMe x            = x + x
doubleUs x y          = x * 2 + y * 2
doublesmallnum x      = if x > 10
                        then x
                        else x*2
ndouble'smallnum x    = succ (doublesmallnum x)
conanO'Brien          = "It's a-me, Conan O'Brein!"
boomBangs xs          = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
