{-# LANGUAGE OverloadedStrings #-}

module Main where

import Numeric.LinearAlgebra

main :: IO ()
main = putStrLn $ show $ vector [1,2,3] * vector [3,0,-2]
