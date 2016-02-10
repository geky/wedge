module Stage where

import Prelude hiding (read, (.), id, const)
import System.FilePath
import Control.Category
import Control.Arrow
import Control.Monad
import Pos
import Result


-- Stage implementation
newtype Stage a b = Stage
    { unstage :: FilePath -> a -> IO (Result (Positional String) b)
    }

stage :: (FilePath -> a -> Result (Positional String) b) -> Stage a b
stage x = Stage $ \fp -> pure . x fp

stageIO :: (FilePath -> a -> IO (Result (Positional String) b)) -> Stage a b
stageIO = Stage

instance Category Stage where
    id = stage $ const ok
    x . y = stageIO $ \fp a -> unstage y fp a <>>= unstage x fp

instance Arrow Stage where
    arr = stage . const . (ok .)
    first x = stageIO $ \fp (a, c) -> liftMM (,c) (unstage x fp a)

instance ArrowChoice Stage where
    left x = stageIO $ \fp e -> case e of
        Left a  -> liftMM Left (unstage x fp a)
        Right a -> returnMM (Right a)


-- Nested binds
liftMM :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
liftMM = liftM . liftM

returnMM :: (Monad m, Monad n) => a -> m (n a)
returnMM = return . return

infixl 1 <>>=
(<>>=) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)
x <>>= y = liftMM y x >>= liftM join . sequence

infixl 1 =<<>
(=<<>) :: (Monad m, Monad t, Traversable t) => (a -> m (t b)) -> m (t a) -> m (t b)
y =<<> x = x <>>= y

infixl 1 <>>
(<>>) :: (Monad m, Monad t, Traversable t) => m (t a) -> m (t b) -> m (t b)
x <>> y = x <>>= \_ -> y


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
read :: Stage () [String]
read = stageIO (\fp _ -> liftM ok (readFile fp)) >>^ lines

write :: Stage (FilePath, [String]) ()
write = second unlines ^>> stageIO (\_ -> liftM ok . uncurry writeFile)

dump :: Show a => String -> Stage a a
dump x = pass $ (stage $ \fp a -> ok (fp -<.> x, [show a])) >>> write


-- Operation on stages
pipe :: FilePath -> a -> Stage a b -> IO (Result (Positional String) b)
pipe fp a x = unstage x fp a

