import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant.Rep


equals :: Eq a => a -> Predicate a
equals = tabulate . (==)

lessThan :: Ord a => a -> Predicate a
lessThan = tabulate . (>=)

(>&&<) :: Predicate a -> Predicate a -> Predicate a
(>&&<) = divide (\x -> (x, x))

infixr 3 >&&< 

notC :: Predicate Bool -> Predicate Bool
notC fa = tabulate (not . (index fa))

(>||<) :: Predicate a -> Predicate a -> Predicate a
(>||<) l r = tabulate (\a -> index l a || index r a)

infixr 2 >||< 


data Person =
  Person { name :: String, age :: Time, address :: String }


nameIsJohn :: Predicate Person
nameIsJohn = contramap name $ equals "John"

data Time = Years Int | Months Int deriving (Read, Show)

inMonths :: Time -> Int
inMonths (Months x) = x
inMonths (Years y) = y * 12



isYoung :: Predicate Time
isYoung =
    choose (\t -> case t of
                      Years x -> Left x
                      Months x -> Right x)
           (tabulate (<60))
           (tabulate (<720))



isAYoungJohnOrBabyKelly :: Predicate Person
isAYoungJohnOrBabyKelly =    nameIsJohn               >&&<  age >$< isYoung 
                       >||<  name >$< equals "Kelly"  >&&<  age >$< (inMonths >$< lessThan 13)

main :: IO ()
main = do

  p <- Person <$> (putStrLn "Enter the name: " >> getLine) <*> (putStrLn "Enter the age in months: " >> ((Months . read) <$> getLine)) <*> (putStrLn "Enter the address: " >> getLine) 
  if getPredicate isAYoungJohnOrBabyKelly p then
    putStrLn "Nice! This person is a young John, or a baby Kelly!"
   else
    putStrLn "lol whatever"

