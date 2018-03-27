{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RFC.Data.ListMoveDirection (
  module RFC.Data.ListMoveDirection
) where

import           Data.Aeson         as Aeson
import           Data.Text          as Text
import           RFC.Prelude

#ifndef GHCJS_BROWSER

import           RFC.Servant.ApiDoc (ToSchemaRFC)
import           Servant.Docs

instance ToSample ListMoveDirection where
  toSamples _ =
    [ ("Up/forward/towards head", TowardsHead)
    , ("Down/backward/towards tail", TowardsTail)
    ]

instance ToSchemaRFC ListMoveDirection where

#endif

moveInList :: (Eq a) => a -> ListMoveDirection -> [a] -> [a]
moveInList target dir lst =
  case (lst,dir) of
    ([], _)                   -> []
    ([_], _)                  -> lst
    ((a:b:rest), TowardsHead) | b == target -> b:a:rest
    ((a:b:rest), TowardsTail) | a == target -> b:a:rest
    ((_:b:rest), _)           -> moveInList target dir (b:rest)


data ListMoveDirection = TowardsHead | TowardsTail
  deriving (Show,Eq,Ord,Enum,Bounded,Generic,Typeable)

instance FromJSON ListMoveDirection where
    parseJSON = withText "ListMoveDirection" $ \t -> do
      let head = return TowardsHead
      let tail = return TowardsTail
      case Text.strip $ Text.toUpper t of
        "UP"           -> head
        "DOWN"         -> tail

        "FORWARD"      -> head
        "BACKWARD"     -> tail

        "FRONT"        -> head
        "BACK"         -> tail

        "-1"           -> head
        "+1"           -> tail

        "-"            -> head
        "+"            -> tail

        "TOWARDSSTART" -> head
        "TOWARDSTART"  -> head
        "TOWARDSEND"   -> tail

        "TOWARDSFRONT" -> head
        "TOWARDSBACK"  -> tail

        "TOWARDSHEAD"  -> head
        "TOWARDSTAIL"  -> tail

        "HEADWARDS"    -> head
        "TAILWARDS"    -> tail

        _              -> fail . cs $ Text.append "Could not parse string to direction: " t

instance ToJSON ListMoveDirection where
  toJSON dir = Aeson.String (
    case dir of
      TowardsHead -> "UP"
      TowardsTail -> "DOWN"
    )
