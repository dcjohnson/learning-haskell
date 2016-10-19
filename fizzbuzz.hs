main :: IO ()
main = foldl (>>) (return ()) ([io | x <- [1..100], let io = case (x `mod` 3, x `mod` 5) of
                                                               (0, 0) -> putStrLn "fizzbuzz"
                                                               (0, _) -> putStrLn "fizz"
                                                               (_, 0) -> putStrLn "buzz"
                                                               _ -> return ()])

