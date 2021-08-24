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

maxX = 1920 * 2 :: Int
maxY = 1080 * 2 :: Int

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

img maxN = renderDrawing maxX maxY (PixelRGBA8 0 0 0 255) $ let
    transformation = mconcat
          [ translate (V2 ((fromIntegral maxX) / 2 - 1000) 0)
          , skewX 0.2
          , rotate (-0.05)
          , scale 0.3 7
          ]

    gradient brightness opacity width =
      linearGradientTexture [ (0, PixelRGBA8 brightness brightness brightness 0)
                            , (0.5, PixelRGBA8 brightness brightness brightness opacity)
                            , (1, PixelRGBA8 brightness brightness brightness 0)
                            ] (V2 (-(fromIntegral maxX) * width) 0) (V2 (fromIntegral maxX * width) 0)
  in withTransformation transformation $ do
    withTexture (gradient 255 50 1) $ fill $ rectangle (V2 0 0) (fromIntegral maxX) (fromIntegral maxY)
    mapM_ (\n -> let
                points = let
                  points' = map (\(V2 x y) -> V2 (x + (fromIntegral n)) y) $ get3np1Points n

                  maxP = maximum (map (\(V2 _ y) -> y) points')
                  in trace (show n ++ " lenght=" ++ show (length points') ++ " max=" ++ show maxP) points'

                morphedPoints = map (\(V2 x y) -> let
                    sign = if even n
                            then -1
                            else 1
                  in V2 (sign * y) x) (points)
              in do
                withTexture (gradient 255 50 0.5) $
                  stroke 0.3 JoinRound (CapRound, CapRound) $ polyline $ init morphedPoints

                withTexture (getColor maxN n) $
                  fill $
                  polygon morphedPoints
          ) (reverse [2..maxN])
    withTexture (gradient 0 200 1) $ fill $ rectangle (V2 0 0) (-(fromIntegral maxX)) (fromIntegral maxY)
    withTexture (uniformTexture $ PixelRGBA8 255 255 255 50) $ stroke 17 JoinRound (CapRound, CapRound) $ line (V2 0 0) (V2 0 2160)

main :: IO ()
main = do
  putStrLn "run..."
  writePng "3np1.png" (img 215)
  putStrLn "...done"