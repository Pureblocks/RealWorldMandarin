module Database.Beam.Extended
    ( runSelectOne
    ) where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions
import Data.Maybe (fromJust)

runSelectOne :: FromBackendRow Postgres a => Connection -> SqlSelect Postgres a -> IO a
runSelectOne conn q = runBeamPostgres conn $ fromJust <$> runSelectReturningOne q