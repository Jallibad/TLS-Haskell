module Main where

import Criterion.Main
import Utility.UInt

main :: IO ()
main = defaultMain [
	bgroup "fromInteger"
		[ bench "1" $ whnf (fromInteger :: Integer -> UInt 64) 1
		, bench "10" $ whnf (fromInteger :: Integer -> UInt 64) 10
		, bench "100" $ whnf (fromInteger :: Integer -> UInt 64) 100
		, bench "1000" $ whnf (fromInteger :: Integer -> UInt 64) 1000
		, bench "10000" $ whnf (fromInteger :: Integer -> UInt 64) 10000
		, bench "100000" $ whnf (fromInteger :: Integer -> UInt 64) 100000
		]
	]