{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}



class D t where
  polyAdd1 :: (Int ~ a) => a->t

instance {-# OVERLAPS #-} (Int ~ a) => D a where
  polyAdd1 x = x

instance ((Int ~ a), D b) => D (a->b) where
  polyAdd1 x y = polyAdd1 (x+y)

class N t where
  polyAdd :: t
  
instance {-# OVERLAPS #-} (Int ~ a) => N a where
  polyAdd = 0

instance (Int ~ a, D b) => N (a->b) where
  polyAdd = polyAdd1



class LD t e where
  polyList1 :: ([e] ~ a) => a->t

instance {-# OVERLAPS #-} ([e] ~ a) => LD a e where
  polyList1 x = x

instance (e ~ a, LD b e) => LD (a->b) e where
  polyList1 x y = polyList1 (x ++ [y])

polyList = polyList1 []


main = do
 print $ (polyList 1 2 3 :: [Int])
