#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [MemoTrie Rasterific])" -i runhaskell

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.MeshPatch
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Data.MemoTrie
import qualified Data.Vector as V
import Debug.Trace (trace)

main :: IO ()
main = let
    maxN = 215
    maxX = 1920 * 2 :: Int
    maxY = 1080 * 2 :: Int

    transformation = mconcat
          [ translate (V2 ((fromIntegral maxX) / 2 - 1000) 0)
          , skewX 0.2
          , rotate (-0.05)
          , scale 0.3 7
          ]

    img = renderDrawing maxX maxY (PixelRGBA8 0 0 0 255) $
      withTransformation transformation $ do
        withTexture (linearGradientTexture [(0, PixelRGBA8 255 255 255 70) ,(1, PixelRGBA8 255 255 255 0)] (V2 0 0) (V2 (fromIntegral maxX) 0)) $ fill $ rectangle (V2 0 0) (fromIntegral maxX) (fromIntegral maxY)
        mapM_ (\n -> do
                  let points = trace (show n) (get3np1Points n)

                  let sign = if even n
                             then -1
                             else 1

                  let poly = polygon (map (\(V2 i n) -> V2 (sign * n) i) points)

                  withTexture (getColor maxN n) $
                    fill $
                    poly

              ) (reverse [1..maxN])
        withTexture (linearGradientTexture [(0, PixelRGBA8 0 0 0 200) ,(1, PixelRGBA8 0 0 0 0)] (V2 0 0) (V2 (-3000) 0)) $ fill $ rectangle (V2 0 0) ( (-1) * (fromIntegral maxX)) (fromIntegral maxY)
        withTexture (uniformTexture $ PixelRGBA8 255 255 255 50) $ stroke 17 JoinRound (CapRound, CapRound) $ line (V2 0 0) (V2 0 2160)
  in do
        putStrLn "run..."
        writePng "3np1.png" img
        putStrLn "...done"

getColor :: Int -> Int -> Texture PixelRGBA8
getColor max n = let
  val = floor (((fromIntegral n) / (fromIntegral max)) * 255)
  in uniformTexture $ PixelRGBA8 val (255 - val) 255 60

get3np1Points :: Int -> [Point]
get3np1Points = let

  next :: Int -> Int
  next n | even n   = n `div` 2
          | otherwise = 3 * n + 1
  mNext :: Int -> Int
  mNext = memo next

  get3np1Points' :: Int -> Int -> [V2 Int]
  get3np1Points' i 1  = [V2 (i - 1) 0, V2 0 0]
  get3np1Points' i n = (V2 (i - 1 ) (n - 1 )) : (get3np1Points' (i + 1) (mNext n))
  in \startN -> map (\(V2 i n) -> V2 (fromIntegral (i + startN)) (fromIntegral n)) $ get3np1Points' 0 startN
