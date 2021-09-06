module Lib where

data Task m
  = Namespace String [Task m]
  | Task String (m ())

-- https://github.com/capistrano/capistrano/blob/master/lib/capistrano/tasks/deploy.rake
deploy :: Monad m => [Task m]
deploy =
  [ Namespace "deploy"
      [ Task "starting" $ pure ()
      , Task "print_config_variables" $ pure ()
      , Task "updating" $ pure ()
      , Task "reverting" $ pure ()
      , Task "publishing" $ pure ()
      , Task "finishing" $ pure ()
      , Task "finishing_rollback" $ pure ()
      , Task "finished" $ pure ()
      , Task "check" $ pure ()
      , Namespace "check"
          [ Task "directories" $ pure ()
          , Task "linked_dirs" $ pure ()
          , Task "make_linked_dirs" $ pure ()
          , Task "linked_files" $ pure ()
          ]
      , Namespace "symlink"
          [ Task "release" $ pure ()
          , Task "shared" $ pure ()
          , Task "linked_dirs" $ pure ()
          , Task "linked_files" $ pure ()
          ]
      , Task "cleanup" $ pure ()
      , Task "cleanup_rollback" $ pure ()
      , Task "log_revision" $ pure ()
      , Task "revert_release" $ pure ()
      , Task "new_release_path" $ pure ()
      , Task "rollback_release_path" $ pure ()
      , Task "set_current_revision" $ pure ()
      , Task "set_previous_revision" $ pure ()
      , Task "restart" $ pure ()
      , Task "failed" $ pure ()
      ]

  ]
