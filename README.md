# qsc
import Data.Set

data  Foo = Foo {left :: Set(Double),right :: Set(Double)}
               deriving (Show)

instance Ord Foo where
  lhs <= rhs = (findMax (left lhs) < ((findMin(right rhs) + findMax (left rhs)) / 2 )) && (findMin (right rhs) > ((findMin(right lhs) + findMax (left lhs)) / 2) )

instance Eq Foo where
  lhs == rhs = (lhs <= rhs) && (rhs <= lhs)

instance Num Foo where
  (+) lhs rhs = Foo (union singleton(findMax (left lhs) + (findMin(right rhs) + findMax (left rhs)) / 2) singleton(findMax (left rhs) + (findMin(right lhs) + findMax (left lhs)) / 2)) (union singleton(findMin (right lhs) + (findMin (right rhs) + findMax (left rhs)) / 2) singleton(findMin (right rhs) + (findMin (right lhs) + findMax (left lhs)) / 2)) 
 
