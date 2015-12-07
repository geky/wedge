module Rule where

type Rule t a = [t] -> (a, [t])

($~) :: (a -> b) -> Rule t a -> Rule t b
($~) f a ts = (f a', ts')
  where (a', ts') = a ts

(*~) :: Rule t (a -> b) -> Rule t a -> Rule t b
(*~) f a ts = (f' a', ts'')
  where
    (f', ts' ) = f ts
    (a', ts'') = a ts'

(~>) :: Rule t a -> (a -> Rule t b) -> Rule t b
(~>) a f ts = uncurry f $ a ts

run :: Rule t a -> [t] -> [a]
run _ [] = []
run r ts = a : run r ts'
  where (a, ts') = r ts

unexpected :: (Show t) => Rule t a
unexpected (t:_) = error $ "unexpected " ++ show t
unexpected _     = error $ "unexpected end of input"

