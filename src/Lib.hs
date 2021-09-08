module Lib where

import Control.Exception
import Control.Monad.Writer
import Data.List

data Task m
  = Namespace String [Task m]
  | Task String ((String -> m ()) -> m ())

data TaskException = TaskNotFoundException
  deriving Show

instance Exception TaskException

runDeployStarting :: IO ()
runDeployStarting =
  let tasks = listTasks [] $ execWriter deploy
  in runTask tasks "deploy:starting"

printTasks :: [(String, (String -> IO ()) -> IO ())] -> IO ()
printTasks tasks = mapM_ (putStrLn . fst) tasks

runTask :: [(String, (String -> IO ()) -> IO ())] -> String -> IO ()
runTask tasks name =
  case lookup name tasks of
    Nothing -> throwIO TaskNotFoundException
    Just f -> f $ runTask tasks

listTasks :: [String] -> [Task IO] -> [(String, (String -> IO ()) -> IO ())]
listTasks prefix = mconcat . fmap listTask
  where
    listTask (Namespace name tasks) = listTasks (prefix <> [name]) tasks
    listTask (Task name f) = [(intercalate ":" $ prefix <> [name], f)]

-- EDSL

namespace :: String -> Writer [Task m] () -> Writer [Task m] ()
namespace name tasks = tell [Namespace name $ execWriter tasks]

task :: String -> ((String -> m ()) -> m ()) -> Writer [Task m] ()
task name f = tell [Task name f]

-- https://github.com/capistrano/capistrano/blob/master/lib/capistrano/tasks/deploy.rake
deploy :: Writer [Task IO] ()
deploy =
  namespace "deploy" $ do
    task "starting" $ \invoke -> do
      putStrLn "Running deploy:starting"
      invoke "deploy:check"
      invoke "deploy:set_previous_revision"

    task "check" $ \invoke -> do
      putStrLn "Running deploy:check"
      invoke "deploy:check:directories"
      invoke "deploy:check:linked_dirs"
      invoke "deploy:check:make_linked_dirs"
      invoke "deploy:check:linked_files"

    namespace "check" $ do
      task "directories" $ \invoke ->
        putStrLn "Running deploy:check:directories"

      task "linked_dirs" $ \invoke ->
        putStrLn "Running deploy:check:linked_dirs"

      task "make_linked_dirs" $ \invoke ->
        putStrLn "Running deploy:check:make_linked_dirs"

      task "linked_files" $ \invoke ->
        putStrLn "Running deploy:check:linked_files"

    task "set_previous_revision" $ \invoke ->
      putStrLn "Running deploy:set_previous_revision"
