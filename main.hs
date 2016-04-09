import Data.List

main = do
    let s = State False [('a', State False [('c', State True []), ('b', State False [('c', State True [])])])]
    print "Please enter a string to test."
    str <- getLine
    print $ eval str s

data State = State IsTerm Transitions
type IsTerm = Bool
type Transitions = [(Char, State)]

class Transition a where
    (==>) :: a -> Char -> Maybe a
    term :: a -> IsTerm
    transitions :: a -> Transitions

instance Transition State where
    (State term trans) ==> transChar = lookup transChar trans
    term (State term _) = term
    transitions (State _ trans) = trans

isEmpty :: Maybe a -> Bool
isEmpty Nothing = True
isEmpty _ = False

extJust :: Maybe a -> a
extJust (Just a) = a

eval :: String -> State -> Bool
eval str state = do
    if (length str == 0) then
        term state
    else
        let new = state ==> (str !! 0)
        in (not (isEmpty new)) && (eval (tail str) (extJust new))
