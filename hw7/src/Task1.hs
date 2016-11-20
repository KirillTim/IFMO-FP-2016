{-# LANGUAGE OverloadedStrings #-}

module Task1 where

import           Data.Array.IO
import           Data.IORef
import qualified Data.Text          as T
import           System.Environment
import           System.IO
import Control.Applicative ((<$>))

readFileLines :: T.Text -> IO[T.Text]
readFileLines fileName = fmap (fmap T.pack) $ lines <$> readFile (T.unpack fileName)

splitToPair :: T.Text -> T.Text -> (T.Text, T.Text)
splitToPair delim str = (head arr, T.unwords $ tail arr)
  where arr = T.splitOn delim str

getProperties :: T.Text -> IO [(T.Text, T.Text)]
getProperties fileName
  | T.null fileName = return []
  | otherwise       = fmap (splitToPair "=") <$> readFileLines fileName

listToIOArray :: [a] -> IO (IOArray Int a)
listToIOArray l = newListArray (0, length l - 1) l


propToStr :: (T.Text, T.Text) -> String
propToStr p = T.unpack (fst p) ++ "=" ++ T.unpack (snd p)

action :: IOArray Int (T.Text, T.Text) -> IO ()
action vals = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case splitToPair " " (T.pack input) of
    ("I", line) -> do oldVals <- getElems vals
                      pair <- newIORef $ splitToPair " " line
                      ref <- readIORef pair
                      if elem (fst ref) $ map fst oldVals then
                        do putStrLn $ "key " ++ T.unpack (fst ref) ++ " already exists"
                           action vals
                      else
                        do newVals <- listToIOArray $ oldVals ++ [ref]
                           putStrLn "inserted"
                           action newVals

    ("U", line) -> do oldVals <- getElems vals
                      pair <- newIORef $ splitToPair " " line
                      a1 <- readIORef pair
                      newVals <- listToIOArray $ map (\p -> if fst p == fst a1 then
                                                                      (fst p, snd a1)
                                                                      else p) oldVals
                      putStrLn $ "key " ++ show (fst a1) ++ " had been updated"
                      action newVals


    ("W", fileName) -> do oldVals <- getElems vals
                          written <- newIORef $ map propToStr oldVals
                          w0 <- readIORef written
                          withFile (T.unpack fileName) WriteMode $ \f -> mapM_ (hPutStrLn f) w0
                          putStrLn $ "File '" ++ T.unpack fileName ++ "' had been written"
                          action vals

    ("L", _) -> do oldVals <- getElems vals
                   mapM_ (putStrLn . propToStr)  oldVals
                   action vals
    ("Q", _) -> return ()
    (cmd, _) -> do putStrLn $ "Unknown command: " ++ T.unpack cmd
                   action vals

main' :: IO()
main' = do args  <- getArgs
           let fileName = if null args then [] else head args
           props <- getProperties $ T.pack fileName
           vals <- listToIOArray props
           putStr "\nInteractive options:\n\
                                        \\t I <key> <value>\t : insert new value\n\
                                        \\t U <key> <value>\t : modify value\n\
                                        \\t W <file>\t\t : write to file\n\
                                        \\t L list all properties\n\
                                        \\t*Q\t\t\t : quit\n"
           action vals
