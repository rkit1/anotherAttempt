{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module ClubviRu.Debug.Helpers where
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Text.Printf

terror :: String -> ExpQ
terror s = do
  Loc{..} <- location
  let str :: String
      str = printf "%s at %s:%s" s loc_module (show loc_start)
  [| error $(lift str) |]

tundefined :: ExpQ
tundefined = terror "undefined"