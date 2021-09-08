module Lib where

import Control.Exception
import Data.List

data Task m
  = Namespace String [Task m]
  | Task String ((String -> m ()) -> m ())

data TaskException = TaskNotFoundException
  deriving Show

instance Exception TaskException

runDeployStarting :: IO ()
runDeployStarting =
  let tasks = listTasks [] deploy
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

-- https://github.com/capistrano/capistrano/blob/master/lib/capistrano/tasks/deploy.rake
deploy :: [Task IO]
deploy =
  [ Namespace "deploy"
      [ Task "starting" $ \invoke -> do
          putStrLn "Running deploy:starting"
          invoke "deploy:check"
          invoke "deploy:set_previous_revision"

      , Task "check" $ \invoke -> do
          putStrLn "Running deploy:check"
          invoke "deploy:check:directories"
          invoke "deploy:check:linked_dirs"
          invoke "deploy:check:make_linked_dirs"
          invoke "deploy:check:linked_files"

      , Namespace "check"
          [ Task "directories" $ \invoke ->
              putStrLn "Running deploy:check:directories"

          , Task "linked_dirs" $ \invoke ->
              putStrLn "Running deploy:check:linked_dirs"

          , Task "make_linked_dirs" $ \invoke ->
              putStrLn "Running deploy:check:make_linked_dirs"

          , Task "linked_files" $ \invoke ->
              putStrLn "Running deploy:check:linked_files"
          ]

      , Task "set_previous_revision" $ \invoke ->
          putStrLn "Running deploy:set_previous_revision"
      ]

  ]
