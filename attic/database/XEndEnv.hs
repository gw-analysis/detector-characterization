

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module XEndEnv where

import KAGRADataSource (defineTable)

$(defineTable "xendenv")



