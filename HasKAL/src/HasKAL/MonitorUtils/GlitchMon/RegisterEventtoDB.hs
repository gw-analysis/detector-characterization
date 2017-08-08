


module HasKAL.MonitorUtils.GlitchMon.RegisterEventtoDB
( part'RegisterEventtoDB
, part'RegisterEventtoDBSQLite3
) where


import Control.Monad ((>>=), mapM_)

import HasKAL.MonitorUtils.GlitchMon.Data (TrigParam (..))
import HasKAL.MonitorUtils.GlitchMon.RegisterGlitchEvent (registGlitchEvent2DB)
import HasKAL.MonitorUtils.GlitchMon.RegisterGlitchEventSQLite3 (registGlitchEvent2DBSQLite3)
import HasKAL.MonitorUtils.GlitchMon.Signature


part'RegisterEventtoDB :: [(TrigParam,ID)] -> IO()
part'RegisterEventtoDB x = do print "registering triggered events"
                              mapM_ registGlitchEvent2DB (fst . unzip $ x)


part'RegisterEventtoDBSQLite3 :: [(TrigParam,ID)] -> IO()
part'RegisterEventtoDBSQLite3 x = do print "registering triggered events"
                                     mapM_ registGlitchEvent2DBSQLite3 (fst . unzip $ x)
