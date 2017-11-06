module RFC.Data.ListMoveDirection (
  module RFC.Data.ListMoveDirection
) where

import RFC.Prelude
import RFC.Servant
import RFC.JSON
import Data.Text as Text
import Data.Aeson as Aeson

data ListMoveDirection = TowardsHead | TowardsTail
  deriving (Show,Eq,Generic,Typeable)

instance ToSample ListMoveDirection where
  toSamples _ = noSamples

instance FromJSON ListMoveDirection where
    parseJSON = withText "ListMoveDirection" $ \t -> do
      let head = return TowardsHead
      let tail = return TowardsTail
      case Text.strip $ Text.toUpper t of
        "UP" -> head
        "DOWN" -> tail

        "FORWARD" -> head
        "BACKWARD" -> tail

        "FRONT" -> head
        "BACK" -> tail

        "-1" -> head
        "+1" -> tail

        "TOWARDSSTART" -> head
        "TOWARDSTART" -> head
        "TOWARDSEND" -> tail

        "TOWARDSFRONT" -> head
        "TOWARDSBACK" -> tail

        "TOWARDSHEAD" -> head
        "TOWARDSTAIL" -> tail

        _ -> fail . cs $ Text.append "Could not parse string to direction: " t

instance ToJSON ListMoveDirection where
  toJSON dir = Aeson.String (
    case dir of
      TowardsHead -> "UP"
      TowardsTail -> "DOWN"
    )
