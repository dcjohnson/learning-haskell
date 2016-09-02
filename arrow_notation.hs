main :: IO ()
main = do
  print $ get $ test 9

test :: Integer -> Maybe Integer
test i = do
  x <- may i
  may $ x + 1

may :: Integer -> Maybe Integer
may i = Just i

get :: Maybe Integer -> Integer
get Nothing = 0
get (Just i) = i
