module Stage where

import Prelude hiding (read, (.), id, const)
import System.FilePath
import Control.Category
import Control.Arrow


-- Stage implementation
type Stage a b = Kleisli IO a b

stage :: (a -> b) -> Stage a b
stage f = arr f

stageIO :: (a -> IO b) -> Stage a b
stageIO = Kleisli


-- More arrow operations
pass :: Arrow w => w a b -> w a a
pass w = id &&& w >>^ fst

const :: Arrow w => a -> w b a
const w = arr $ \_ -> w

mapA :: ArrowChoice w => w a b -> w [a] [b]
mapA w 
  =   (\case [] -> Left (); (x:xs) -> Right (x,xs))
  ^>> const [] ||| (w *** mapA w >>^ uncurry (:))

mapA_ :: ArrowChoice w => w a b -> w [a] ()
mapA_ w = mapA w >>> const ()


-- IO specific operations
read :: Stage FilePath (FilePath, [String])
read = id &&& (stageIO readFile >>^ lines)

write :: Stage (FilePath, [String]) ()
write = second unlines ^>> stageIO (uncurry writeFile)

dump :: Show a => String -> Stage (FilePath, a) (FilePath, a)
dump x = pass $ (-<.> x) *** pure.show ^>> write


-- Operations on stages
pipe :: a -> Stage a b -> IO b
pipe = flip runKleisli


