main :: IO ()
main = do
  print $ get test

test :: Maybe Integer
test = do
  x <- may 6
  may x

may :: Integer -> Maybe Integer
may i = Just i

get :: Maybe Integer -> Integer
get Nothing = 0
get (Just i) = i
