# qsc
import Data.Set

data  Foo = Foo {left :: Set(Double),right :: Set(Double)}
               deriving (Show)
sum1 :: Set a -> Foo a -> Set a
sum1 x y = singleton(findMax x + (findMin(right y) + findMax (left y)) / 2)

sum2 :: Set a -> Foo a -> Set a
sum2 x y = singleton(findMin x + (findMin(right y) + findMax (left y)) / 2)

instance Ord Foo where
  lhs <= rhs = (findMax (left lhs) < ((findMin(right rhs) + findMax (left rhs)) / 2 )) && (findMin (right rhs) > ((findMin(right lhs) + findMax (left lhs)) / 2) )

instance Eq Foo where
  lhs == rhs = (lhs <= rhs) && (rhs <= lhs)

instance Num Foo where
  (+) lhs rhs = Foo (union (map (sum1 u rhs) (left lhs)) (map (sum1 u lhs) (left rhs))) (union (map (sum2 u rhs) (right lhs)) (map (sum1 u lhs) (right rhs)))
 
