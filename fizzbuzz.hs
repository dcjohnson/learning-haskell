main :: IO ()
main = foldl (\a c -> do
                 a
                 c) (return ()) ([io | x <- [1..100], let io = if x `mod` 3 == 0 && x `mod` 5 == 0
                                                               then putStrLn"fizzbuzz"
                                                               else if x `mod` 3 == 0
                                                                    then putStrLn "fizz"
                                                                    else if x `mod` 5 == 0
                                                                         then putStrLn "buzz"
                                                                         else return ()])
