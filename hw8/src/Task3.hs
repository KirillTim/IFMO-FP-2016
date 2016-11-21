{-# LANGUAGE OverloadedStrings #-}

module Task1 where

import           Control.Applicative        ((<$>))
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (EitherT (..), left)
import           Control.Monad.Trans.State  (StateT (..), get, put)
import           Data.Array.IO              (IOArray, getElems, newListArray)
import           Data.IORef                 (newIORef, readIORef)
import qualified Data.Text                  as T
import           System.Environment         (getArgs)
import           System.IO                  (IO, IOMode (..), hFlush, hPutStrLn,
                                             stdout, withFile)

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


propToStr :: (T.Text, T.Text) -> [Char]
propToStr p = T.unpack (fst p) ++ "=" ++ T.unpack (snd p)

-- HAAAARD!!
loop :: (Monad m) => EitherT e m a -> m e
loop = fmap (either id id) . runEitherT . forever

quit :: (Monad m) => e -> EitherT e m r
quit = left

type IOArrOfProp = IOArray Int (T.Text, T.Text)

action :: StateT IOArrOfProp IO ()
action = loop $ do
  vals <- lift get
  liftIO $ putStr "> "
  liftIO $ hFlush stdout
  input <- liftIO getLine
  case splitToPair " " (T.pack input) of
    ("I", line) -> do oldVals <- liftIO $ getElems vals
                      pair <- liftIO $ newIORef $ splitToPair " " line
                      ref <- liftIO $ readIORef pair
                      if elem (fst ref) $ map fst oldVals then
                        do liftIO $ putStrLn $ "key " ++ T.unpack (fst ref) ++ " already exists"
                           lift $ put vals
                      else
                        do newVals <- liftIO $ listToIOArray $ oldVals ++ [ref]
                           liftIO $ putStrLn "inserted"
                           lift $ put newVals

    ("U", line) -> do oldVals <- liftIO $ getElems vals
                      pair <- liftIO $ newIORef $ splitToPair " " line
                      a1 <- liftIO $ readIORef pair
                      newVals <- liftIO $ listToIOArray $ map (\p -> if fst p == fst a1 then
                                                                      (fst p, snd a1)
                                                                      else p) oldVals
                      liftIO $ putStrLn $ "key " ++ show (fst a1) ++ " had been updated"
                      lift $ put newVals


    ("W", fileName) -> do oldVals <- liftIO $ getElems vals
                          written <- liftIO $ newIORef $ map propToStr oldVals
                          w0 <- liftIO $ readIORef written
                          liftIO $ withFile (T.unpack fileName) WriteMode $ \f -> mapM_ (hPutStrLn f) w0
                          liftIO $ putStrLn $ "File '" ++ T.unpack fileName ++ "' had been written"
                          lift $ put vals

    ("L", _) -> do oldVals <- liftIO $ getElems vals
                   liftIO $ mapM_ (putStrLn . propToStr)  oldVals
                   lift $ put vals
    ("Q", _) -> quit ()
    (cmd, _) -> do liftIO $ putStrLn $ "Unknown command: " ++ T.unpack cmd
                   lift $ put vals

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
           void $ runStateT action vals
