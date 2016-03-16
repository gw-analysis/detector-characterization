


module GlitchMon.RegisterEventtoDB
(part'RegisterEventtoDB
) where


import Control.Monad ((>>=), mapM_)

import GlitchMon.Data (TrigParam (..))
import GlitchMon.RegisterGlitchEvent (registGlitchEvent2DB)
import GlitchMon.Signature


part'RegisterEventtoDB :: [(TrigParam,ID)] -> IO()
part'RegisterEventtoDB x = mapM_ registGlitchEvent2DB (fst . unzip $ x)



