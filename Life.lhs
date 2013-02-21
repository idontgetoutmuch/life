It's part of Haskell folklore that the archetypal example for comonads is Conway's game of life. Here's an implementation using arrays.

> {-# OPTIONS_GHC -Wall                    #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults  #-}

> import Diagrams.Prelude
> import Diagrams.Backend.Cairo.CmdLine
> import Data.Array
> import Data.List

The usual comonad class:

> class Comonad c where
>   coreturn :: c a -> a
>   (=>>) :: c a -> (c a -> b) -> c b

This will become our two dimensional grid when we concretize it.

> data PointedArray i a = PointedArray i (Array i a)
>   deriving Show

As usual we make this into a functor by using the functor instance of the underlying array.

> instance Ix i => Functor (PointedArray i) where
>   fmap f (PointedArray i a) = PointedArray i (fmap f a)

An array with a distinguished element is a comonad in which the cobind
updates each element of the array simultaneously.

> instance Ix i => Comonad (PointedArray i) where
>   coreturn (PointedArray i a) = a!i
>   (PointedArray i a) =>> f =
>     PointedArray i (listArray (bounds a)
>                    (map (f . flip PointedArray a) (range $ bounds a)))

Let's have a small grid size to demonstrate the so called blinker.

> mMax, nMax :: Int
> mMax = 5
> nMax = 5

A cell is either dead or alive.

> data Liveness =
>   Dead | Alive
>   deriving (Eq, Show)

Let's be explicit about our neighbours.

> data Neighbours a = Neighbours { north     :: a
>                                , northEast :: a
>                                , east      :: a
>                                , southEast :: a
>                                , south     :: a
>                                , southWest :: a
>                                , west      :: a
>                                , northWest :: a
>                                }
>                   deriving Show

We can't have an infinite grid with an array but we can make our game
of life take place on a torus rather than the plane. This way we don't
have problems with boundary conditions.

> neighbours :: Int -> Int -> PointedArray (Int, Int) a -> Neighbours a
> neighbours mMax nMax (PointedArray (i, j) x) =
>   Neighbours
>     {
>       north     = x!(i,                  (j + 1) `mod` nMax)
>     , northEast = x!((i + 1) `mod` mMax, (j + 1) `mod` nMax)
>     , east      = x!((i + 1) `mod` mMax, j)
>     , southEast = x!((i + 1) `mod` mMax, (j - 1) `mod` nMax)
>     , south     = x!(i,                  (j - 1) `mod` nMax)
>     , southWest = x!((i - 1) `mod` mMax, (j - 1) `mod` nMax)
>     , west      = x!((i - 1) `mod` mMax, j)
>     , northWest = x!((i - 1) `mod` mMax, (j + 1) `mod` nMax)
>     }

> neighboursKlein :: Int -> Int -> PointedArray (Int, Int) a -> Neighbours a
> neighboursKlein mMax nMax (PointedArray (i, j) x) =
>   Neighbours
>     {
>       north     = north' i j
>     , northEast = northEast' i j
>     , east      = x!((i + 1) `mod` mMax, j)
>     , southEast = x!((i + 1) `mod` mMax, (j - 1) `mod` nMax)
>     , south     = x!(i,                  (j - 1) `mod` nMax)
>     , southWest = x!((i - 1) `mod` mMax, (j - 1) `mod` nMax)
>     , west      = x!((i - 1) `mod` mMax, j)
>     , northWest = x!((i - 1) `mod` mMax, (j + 1) `mod` nMax)
>     }
>   where
>     north'      i j | j < nMax - 1  =  x!(i,                 j + 1)
>                     | otherwise     =  x!(mMax - i - 1,          0)
>     northEast' i j | j < nMax - 1  =  x!((i + 1) `mod` mMax, j + 1)
>                    | otherwise     =  x!(mMax - i - 2,           0)


> toList :: Neighbours a -> [a]
> toList (Neighbours x1 x2 x3 x4 x5 x6 x7 x8) = x1:x2:x3:x4:x5:x6:x7:x8:[]

> numNeighbours :: PointedArray (Int, Int) Liveness -> Int
> numNeighbours p = length $ filter (== Alive) $ toList $ neighbours mMax nMax p

Now we can implement the rules.

> f :: PointedArray (Int, Int) Liveness -> Liveness
> f p@(PointedArray (i, j) x)
>   |  x!(i, j) == Alive && (numNeighbours p < 2)
>   = Dead
> f p@(PointedArray (i, j) x)
>   |  x!(i, j) == Alive && (numNeighbours p `elem` [2, 3])
>   = Alive
> f p@(PointedArray (i, j) x)
>   |  x!(i, j) == Alive && (numNeighbours p > 3)
>   = Dead
> f p@(PointedArray (i, j) x)
>   |  x!(i, j) == Dead && (numNeighbours p == 3)
>   = Alive
> f   (PointedArray (i, j) x)
>   = x!(i, j)

We create a single blinker on our torus.

> genesis :: PointedArray (Int, Int) Liveness
> genesis = PointedArray (0, 0) xs
>           where
>             ys = listArray ((0, 0), (mMax - 1, nMax - 1)) $ repeat Dead
>             xs = ys // [((0, 0), Alive), ((0, 1), Alive), ((0, 2), Alive)]

> glider :: PointedArray (Int, Int) Liveness
> glider = PointedArray (0, 0) xs
>   where
>     ys = listArray ((0, 0), (mMax - 1, nMax - 1)) $ repeat Dead
>     xs = ys // [ ((2, 4), Alive)
>                , ((3, 3), Alive)
>                , ((1, 2), Alive)
>                , ((2, 2), Alive)
>                , ((3, 2), Alive)
>                ]

> background :: Diagram Cairo R2
> background = rect 1.2 1.2 # translate (r2 (0.5, 0.5))

> renderLife :: PointedArray (Int, Int) Liveness -> Diagram Cairo R2
> renderLife (PointedArray _ arr) =
>   mconcat $
>   map (\(x, y, c) -> (square (1.0 / (fromIntegral mMax))) # translate (r2 (x, y)) # fc c)
>   [(fromIntegral x / (fromIntegral mMax),
>     fromIntegral y / (fromIntegral nMax),
>     blink x y)
>   | x <- [0, 1 .. mMax - 1]
>   , y <- [0, 1 .. nMax - 1]]
>   where
>     blink x y |  arr!(x, y) == Alive = blue
>               | otherwise            = red

> main = defaultMain $ vcat $ map hcat gridss
>   where
>     grids = map (\x -> background <> renderLife x) $ iterate (=>> f) glider
>
>     gridss = take 5 $ groups 5 grids
>     groups n = unfoldr f
>       where
>         f :: [a] -> Maybe ([a], [a])
>         f [] = Nothing
>         f xs = Just (take n xs, drop n xs)