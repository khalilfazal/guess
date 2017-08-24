{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concatenative     (triAp)
import Control.Concurrent.STM    (readTChan)
import Control.Monad.STM         (atomically)
import Data.Composition          ((.:.))
import Data.Function             (on)

import Graphics.Vty.Config       (standardIOConfig)

import Graphics.Vty.Input.Events (Event (EvKey), Key (KDown, KUp))
import Graphics.Vty.Input        (Input (Input))
import Graphics.Vty.Input        (_eventChannel, inputForConfig, shutdownInput)

import Numeric                   (readFloat, fromRat)
import System.Environment        (getArgs)

import System.IO                 (BufferMode (NoBuffering))
import System.IO                 (hSetEcho, hSetBuffering, stdin, stdout)

import Text.Printf               (printf)

readRational :: String -> Rational
readRational = fst . head . readFloat

showApproxFrac :: Rational -> String
showApproxFrac = show . fromRat

guess :: Input -> Rational -> Rational -> IO ()
guess controller@Input{_eventChannel = keyListener} x y = do
    let n = (x + y) / 2
    (putStrLn .:. triAp showApproxFrac (printf "(%s, %s) -> %s")) x y n

    atomically (readTChan keyListener) >>= \case
        EvKey KUp []   -> guess controller n y
        EvKey KDown [] -> guess controller x n
        _              -> shutdownInput controller

getController :: IO Input
getController = standardIOConfig >>= inputForConfig

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    controller <- getController

    getArgs >>= \case
        [x, y] -> on (guess controller) readRational x y
        _      -> shutdownInput controller
