module Rule where

import Prelude hiding (repeat)


type Rule t a = [t] -> Result t a

data Result t a
  = Accept a [t]
  | Reject   [t]


(~$) :: (a -> b) -> Rule t a -> Rule t b
(~$) f a ts = case a ts of
  Accept a ts -> Accept (f a) ts
  Reject   ts -> Reject       ts

(~*) :: Rule t (a -> b) -> Rule t a -> Rule t b
(~*) f a ts = case f ts of
  Accept f ts -> f ~$ a $ ts
  Reject   ts -> Reject ts

(~|) :: Rule t a -> Rule t a -> Rule t a
(~|) a b ts = case a ts of
  Accept a ts -> Accept a ts
  Reject   ts -> b ts

(~~) :: Rule t a -> (a -> Rule t b) -> Rule t b
(~~) a f ts = case a ts of
  Accept a ts -> f a ts
  Reject   ts -> Reject ts

(~>) :: Rule t a -> Rule t b -> Rule t b
(~>) a b = a ~~ const b

(~<) :: Rule t a -> Rule t b -> Rule t a
(~<) a b = a ~~ \a -> const a ~$ b


repeat, repeat1 :: Rule t a -> Rule t [a]
repeat  r = (:) ~$ r ~* repeat r ~| Accept []
repeat1 r = (:) ~$ r ~* repeat r

delimit, delimit1 :: Rule t a -> Rule t b -> Rule t [a]
delimit  r s = (:) ~$ r ~< s ~* delimit r s ~| Accept []
delimit1 r s = (:) ~$ r ~* delimit r s

terminate, terminate1 :: Rule t a -> Rule t b -> Rule t [a]
terminate  r s = repeat  (r ~< s)
terminate1 r s = repeat1 (r ~< s)

separate, separate1 :: Rule t a -> Rule t b -> Rule t [a]
separate  r s = repeat s ~> delimit  r (repeat1 s) ~< repeat s
separate1 r s = repeat s ~> delimit1 r (repeat1 s) ~< repeat s


unexpected :: Show t => [t] -> a
unexpected (t:_) = error $ "unexpected " ++ (show t)
unexpected _     = error $ "unexpected end of input"

run :: Show t => Rule t a -> [t] -> a
run r ts = case r ts of
  Accept a [] -> a
  Accept _ ts -> unexpected ts
  Reject   ts -> unexpected ts

