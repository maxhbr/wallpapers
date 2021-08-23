#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [MemoTrie Rasterific containers])" -i runhaskell

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.MeshPatch
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Data.MemoTrie
import Data.Set (Set)
import Control.Monad
import qualified Data.Set as S
import qualified Data.Vector as V
import Debug.Trace (trace)

main :: IO ()
main = let
    maxX = 1920 * 2 :: Int
    maxY = 1080 * 2 :: Int

    gradient brightness opacity width =
      linearGradientTexture [ (0, PixelRGBA8 brightness brightness brightness 0)
                            , (0.5, PixelRGBA8 brightness brightness brightness opacity)
                            , (1, PixelRGBA8 brightness brightness brightness 0)
                            ] (V2 (-(fromIntegral maxX) * width) 0) (V2 (fromIntegral maxX * width) 0)

    img1 = renderDrawing maxX maxY (PixelRGBA8 0 0 0 255) $ let
        transformation = mconcat
              [ translate (V2 ((fromIntegral maxX) / 2 - 1000) 0)
              , skewX 0.2
              , rotate (-0.05)
              , scale 0.3 7
              ]
      in withTransformation transformation $ let
        getColor :: Int -> Int -> Texture PixelRGBA8
        getColor _   30  = uniformTexture $ PixelRGBA8 30 90 200 200
        getColor _   41  = uniformTexture $ PixelRGBA8 40 80 200 200
        getColor _   62  = uniformTexture $ PixelRGBA8 50 70 200 200
        getColor _   97  = uniformTexture $ PixelRGBA8 60 60 200 200
        getColor _   145 = uniformTexture $ PixelRGBA8 70 50 200 200
        getColor _   166 = uniformTexture $ PixelRGBA8 80 40 200 200
        getColor _   189 = uniformTexture $ PixelRGBA8 90 30 200 200
        getColor max n   = let
          val = floor (((fromIntegral n) / (fromIntegral max)) * 255)
          in uniformTexture $ PixelRGBA8 val (255 - val) 255 60
      in do
        let maxN = 215
        withTexture (gradient 255 50 1) $ fill $ rectangle (V2 0 0) (fromIntegral maxX) (fromIntegral maxY)
        mapM_ (\n -> let
                    points = let
                      points' = map (\(V2 x y) -> V2 (x + (fromIntegral n)) y) $ get3np1Points n

                      maxP = maximum (map (\(V2 _ y) -> y) points')
                      in trace (show n ++ " lenght=" ++ show (length points') ++ " max=" ++ show maxP) points'

                    sign = if even n
                           then -1
                           else 1
                    morphedPoints = map (\(V2 x y) -> V2 (sign * y) x) (points)

                    poly = polygon morphedPoints
                  in do
                    withTexture (gradient 255 50 0.5) $
                      stroke 0.3 JoinRound (CapRound, CapRound) $ polyline $ init morphedPoints

                    withTexture (getColor maxN n) $
                      fill $
                      poly

              ) (reverse [1..maxN])
        withTexture (gradient 0 200 1) $ fill $ rectangle (V2 0 0) ( -(fromIntegral maxX)) (fromIntegral maxY)
        withTexture (uniformTexture $ PixelRGBA8 255 255 255 50) $ stroke 17 JoinRound (CapRound, CapRound) $ line (V2 0 0) (V2 0 2160)
    img2 = renderDrawing maxX maxY (PixelRGBA8 20 20 20 255) $ let
        maxN = 300
        getColor n ps = let
          lps = length ps
          mps = maximum (map (\(V2 _ y) -> y) ps)
          val = floor (((fromIntegral n) / (fromIntegral maxN)) * 255)
          in uniformTexture $ PixelRGBA8 (val `div` 5) (val `div` 6) val 80
      in do
        withTexture (gradient 255 70 1) $ fill $ rectangle (V2 0 0) (fromIntegral maxX) (fromIntegral maxY)
        withTransformation (mconcat [translate (mkV2 (-800) (maxY + 160)), scale 1 (-1)]) $ do
          mapM_ (\n -> let
                      points = let
                        points' = get3np1Points n
                        maxP = maximum (map (\(V2 _ y) -> y) points')
                        in trace (show n ++ " lenght=" ++ show (length points') ++ " max=" ++ show maxP) points'

                      sign = if even n
                            then -1
                            else 1
                      morphedPoints = map (\(V2 i n) -> V2 (i * 100) (sqrt n * 20)) (points)

                      poly = polygon morphedPoints
                      polyl = polyline $ init morphedPoints
                    in do
                      withTexture (getColor n points) $
                        fill $
                        poly
                      withTexture (uniformTexture $ let
                                      val = floor (((fromIntegral n) / (fromIntegral maxN)) * 255)
                                      in PixelRGBA8 val val val 200) $
                        stroke 0.3 JoinRound (CapRound, CapRound) $ polyl

                ) (reverse [1..maxN])
  in do
        putStrLn "run..."
        -- writePng "3np1.png" img1
        writePng "3np1-2.png" img2
        putStrLn "...done"

get3np1Points :: Int -> [Point]
get3np1Points = let

  next :: Int -> Int
  next n | even n   = n `div` 2
         | otherwise = 3 * n + 1
  mNext :: Int -> Int
  mNext = memo next

  nextL :: Int -> [Int]
  nextL 1 = [1]
  nextL n = n : (mNextL (next n))
  mNextL = memo nextL

  in \startN -> (map (\(x,y) -> V2 (fromIntegral x) (fromIntegral y - 1)) (zip [0..] (nextL startN))) ++ [V2 0 0]

mkV2 :: Num a => Int -> Int -> V2 a
mkV2 i1 i2 = V2 (fromIntegral i1) (fromIntegral i2)

data Edge = Edge Int Int
  deriving (Eq, Show)
data Row
  = Row
  { _idx :: Int
  , _containedNumbers :: Set Int
  , _edges :: [Edge]
  } deriving (Show)
type Rows = [Row]

firstRow = Row 1 (S.fromList [1,2]) [Edge 1 2]

nextRow :: Row -> Row
nextRow lastRow = let
    lastContainedNumbers = _containedNumbers lastRow
    nextContainedNumbers = let
      getContainedNumbersOfEdge (Edge i1 i2) = S.fromList [i1, i2]
      in lastContainedNumbers <> (mconcat $ map getContainedNumbersOfEdge (_edges lastRow))
    iterateEdge (Edge i1 i2) =
      Edge i2 (2 * i2) : (let
                             i2m1 = i2 - 1
                           in if i2m1 `mod` 3 == 0
                           then [Edge i2 (i2m1 `div` 3)]
                           else [])
    nextEdges = filter(\(Edge _ i2) -> not $ i2 `S.member` lastContainedNumbers) $ concatMap iterateEdge (_edges lastRow)
  in Row (_idx lastRow + 1) nextContainedNumbers nextEdges
