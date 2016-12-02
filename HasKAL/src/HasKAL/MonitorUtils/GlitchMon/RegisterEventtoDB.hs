


module HasKAL.MonitorUtils.GlitchMon.RegisterEventtoDB
(part'RegisterEventtoDB
) where


import Control.Monad ((>>=), mapM_)

import HasKAL.MonitorUtils.GlitchMon.Data (TrigParam (..))
import HasKAL.MonitorUtils.GlitchMon.RegisterGlitchEvent (registGlitchEvent2DB)
import HasKAL.MonitorUtils.GlitchMon.Signature


part'RegisterEventtoDB :: [(TrigParam,ID)] -> IO()
part'RegisterEventtoDB x = do print "registering triggered events"
                              mapM_ registGlitchEvent2DB (fst . unzip $ x)


