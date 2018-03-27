{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module RFC.Data.IdAnd
  ( idAndsToMap
  , IdAnd(..)
  , idAndToId
  , idAndToValue
  , valuesToIdAnd
  , idAndToTuple
  , tupleToIdAnd
  , idAndToPair
  , RefMap(..)
  , refMapElems
  , refMapToMap
  ) where

import           RFC.Prelude


import           Data.Aeson        as JSON
import qualified Data.Map          as Map
import qualified Data.UUID.Types   as UUID

#if MIN_VERSION_aeson(1,0,0)
  -- Don't need the backflips for maps
#else
import           Data.Aeson.Types  (Parser, typeMismatch)
-- import           Data.Bitraversable
import qualified Data.HashMap.Lazy as HashMap
#endif

#ifndef GHCJS_BROWSER
import           Control.Lens      hiding ((.=))
import qualified Data.List         as List
import           Data.Proxy        (Proxy (..))
import           Data.Swagger
import           Servant.Docs
#endif

-- |Represents something which has an ID.
newtype IdAnd a = IdAnd (UUID, a)
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype RefMap a = RefMap (Map.Map UUID (IdAnd a))
  deriving (Eq, Ord, Show, Generic, Typeable, FromJSON, ToJSON)

refMapElems :: RefMap a -> [IdAnd a]
refMapElems = Map.elems . refMapToMap

refMapToMap :: RefMap a -> Map.Map UUID (IdAnd a)
refMapToMap (RefMap it) = it

idAndToValue :: IdAnd a -> a
idAndToValue (IdAnd(_,a)) = a

idAndToId :: IdAnd a -> UUID
idAndToId (IdAnd(id,_)) = id

tupleToIdAnd :: (UUID, a) -> IdAnd a
tupleToIdAnd = IdAnd

valuesToIdAnd :: UUID -> a -> IdAnd a
valuesToIdAnd id a = IdAnd(id,a)

idAndToTuple :: IdAnd a -> (UUID, a)
idAndToTuple (IdAnd it) = it

idAndToPair :: IdAnd a -> (UUID, IdAnd a)
idAndToPair idAnd@(IdAnd (id,_)) = (id, idAnd)

idAndsToMap :: [IdAnd a] -> RefMap a
idAndsToMap list = RefMap . Map.fromList $ (\idAnd@(IdAnd(uuid,_)) -> (uuid,idAnd)) <$> list

instance (FromJSON a) => FromJSON (IdAnd a) where
  parseJSON = JSON.withObject "IdAnd" $ \o -> do
    id <- o .: "id"
    value <- o .: "value"
    return $ IdAnd(id, value)

instance (ToJSON a) => ToJSON (IdAnd a) where
  toJSON (IdAnd (id,value)) = object [ "id".=id, "value".=value ]

#if MIN_VERSION_aeson(1,0,0)
  -- Have Mpa instances automatically created
#else
instance (FromJSON a) => FromJSON (Map UUID (IdAnd a)) where
  parseJSON (Object obj) =
      Map.fromList <$> listInParser
    where
      objList :: [(Text, Value)]
      objList = HashMap.toList obj
      die :: Text -> Parser UUID
      die k = fail . cs $ "Could not parse UUID: " <> k
      mapMKey :: Text -> Parser UUID
      mapMKey k = maybe (die k) return $ UUID.fromText k
      mapMVal :: Value -> Parser (IdAnd a)
      mapMVal = parseJSON
      mapPair :: (Text,Value) -> Parser (UUID, IdAnd a)
      mapPair = bimapM mapMKey mapMVal
      parserList :: [Parser (UUID, IdAnd a)]
      parserList = mapPair <$> objList
      listInParser :: Parser [(UUID, IdAnd a)]
      listInParser = sequence parserList

  parseJSON invalid = typeMismatch "Map UUID (IdAnd a)" invalid


instance (ToJSON a) => ToJSON (Map UUID (IdAnd a)) where
  toJSON =
    Object . HashMap.fromList . fmap (\(k,v) -> (UUID.toText k, toJSON v)) . Map.toList

#endif

#ifndef GHCJS_BROWSER
instance (ToSchema a, ToJSON a, ToSample a) => ToSchema (IdAnd a) where
  declareNamedSchema _ = do
    NamedSchema{..} <- declareNamedSchema (Proxy :: Proxy a)
    let aMaybeName =  _namedSchemaName
    aSchema <- declareSchemaRef (Proxy :: Proxy a)
    idSchema <- declareSchemaRef (Proxy :: Proxy UUID)
    let maybeSample = safeHead $ toSamples (Proxy :: Proxy (IdAnd a))
    return . NamedSchema (fmap (\name -> "IdAnd " <> name) aMaybeName) $
      mempty
        & type_ .~ SwaggerObject
        & properties .~ [("id", idSchema), ("value", aSchema)]
        & required .~ ["id", "value"]
        & example .~ (toJSON . snd <$> maybeSample)

instance (ToSchema a, ToJSON a, ToSample a) => ToSchema (RefMap a) where
  declareNamedSchema _ = do
    NamedSchema{..} <- declareNamedSchema (Proxy :: Proxy a)
    let aMaybeName =  _namedSchemaName
    idAndASchema <- declareSchemaRef (Proxy :: Proxy (IdAnd a))
    let maybeSample = safeHead $ toSamples (Proxy :: Proxy (RefMap a))
    return . NamedSchema (fmap (\name -> "RefMap " <> name) aMaybeName) $
      mempty
        & type_ .~ SwaggerObject
        & additionalProperties ?~ idAndASchema
        & example .~ (toJSON . snd <$> maybeSample)

uuidList :: [UUID]
uuidList = List.cycle $ fromMaybe UUID.nil . UUID.fromString <$>
  [ "4fc2ffac-9100-41d6-94e1-c33a545e9ba2"
  , "1046948e-f8c7-4985-a008-fec4938696f4"
  , "83162802-598f-4de3-bd35-6c3fc0433965"
  , "1f628272-cfd8-4d43-ab00-ace5085164cc"
  , "64de6be2-4ca7-4ab7-ba3b-42d5dd0b79e7"
  , "20799832-f67d-4fd7-9435-58f3452f680d"
  , "a65c9782-863f-47cb-a2d4-438b8ade9e14"
  , "6e76c9a0-31f2-4bc7-a7b4-a2bb4a4dc03f"
  , "cc8b25ad-b596-4922-801a-2daeab69fbbf"
  , "59f44f8b-16e2-4839-b455-a1df01c704a5"
  , "7d8423c2-60a5-480f-b8b4-732b832bc80f"
  , "4b9f6e8c-c6c7-47f8-b1b6-13c987c17396"
  , "ae80805a-93bd-4f03-9c1e-d1ef0225b914"
  , "21ec1b66-97b7-47d4-9f08-2bb32ebf8a6f"
  , "799cdc04-aa86-45f9-91ae-24a7b9ea5302"
  , "babde8aa-055f-4936-b693-a32659f7ca63"
  , "99bde639-8ae1-40f5-a447-13531530f06e"
  , "67e7b91c-561f-4307-ae79-f5d7cc37fbe4"
  , "4a158750-2074-4056-9314-77d082f567d2"
  , "acc114a2-3307-4b52-80fb-36db3878b487"
  , "ec2a6366-b693-4b30-a888-de4a7e577c95"
  , "01ee4efb-6ffc-412d-aedc-c0a49a606918"
  , "25c8288d-68c1-4b5b-98a3-8af6932db9dc"
  , "7bc8dd7d-95e7-4bb1-bae9-fe2fb9e60ec9"
  , "9e6614a2-dc4f-4f97-9531-bfdbbdf70f15"
  , "fd0e77a8-fcdd-4c52-8b41-31fb4b137a46"
  , "b0395674-5b36-4ea7-98c3-a65c879cb580"
  , "0a4f6d8e-a72c-428d-8fbb-616510e1c772"
  , "4a01469f-285a-471c-9381-6ebeba00d4d9"
  , "5312022d-be32-4865-8d9b-cda95179ea2e"
  , "f2f72c71-77fd-4df2-a0fe-426f9749b96c"
  , "7cf30079-d3e1-4215-bbde-7ed81db1b6f6"
  , "31e491e4-0e41-45b2-994a-d18063c218ba"
  , "8b364c18-9e46-4776-ac78-ffa4e200104b"
  , "3e5b0676-8258-4e55-a3af-233d5ffac4c7"
  , "90f3d59f-42b8-476f-af4f-30abe465f510"
  , "46ad2bc5-f1bb-47e5-b336-8465eca1c3e1"
  , "8bfd88b5-f0c8-4e80-a25b-509c3d70f72e"
  , "eb9f9756-9431-4a0f-8df4-641f161f9450"
  , "99237a8c-14ae-4da3-a1d1-ee09add55746"
  , "3d474e6c-3402-46f6-8451-be3d6143890a"
  , "60745f1b-d0a3-4754-b958-e1067527b5c4"
  , "bd6dbf99-6c68-4ce4-a34b-daa3c4766487"
  , "ce81cc21-fba5-4b0b-8464-52ac5f4e6fd0"
  , "2c42e8b6-89db-4ea6-87c0-1a63e8908b91"
  , "6bfa8e14-dadd-4866-ae89-c51582622e68"
  , "9e100253-f807-40d1-9eea-73e7b50be390"
  , "ace8ebeb-9302-4a86-b29a-ec3a8aeaa73d"
  , "7c380f96-a427-46bc-af93-f4451d6fc072"
  , "e91a4d88-9565-4392-883e-e1afbf7b4e1d"
  , "1c9534e9-97d6-4fc8-b072-e536fc80e16a"
  , "30adcd09-9bcc-4b8f-be73-8d9d63b9254b"
  , "a27aed7a-0074-4535-8776-573890d4bbe8"
  , "a1342012-e505-4e4b-a245-08b04cb7a36f"
  , "cff3c2d5-01a4-4ccd-bf4c-4f7d6ccef6a6"
  , "10eb2b44-7b9b-4822-9b0b-ee152a506f99"
  , "f434e085-8c73-4832-ba99-e18a0a42a77f"
  , "7f029d88-22b0-4c4f-bee5-b8de9bd4b787"
  , "e71d3104-dee1-4926-8b29-c96955e46fa8"
  , "833bf188-d160-4dfc-a2ff-cb63b273a7dc"
  , "fbe1a7ca-4b5f-4eaf-a064-43de857625de"
  , "2d2daa9c-109c-456a-b94f-c55dd440d938"
  , "785db7db-5221-4ebf-86de-c91e5b3d5438"
  , "e504b605-07f8-4da2-bb5b-a86cdb9d117e"
  , "05cf172b-312b-4078-9bd8-77215b12d0b0"
  , "2b77b9f1-dfbe-4e8a-b710-25df48c2b8c7"
  , "ec402b1f-226e-481b-8e11-e52337a36481"
  , "54ef2193-197e-41b5-83ab-c0e516432f84"
  , "2d7115d1-89a9-4b83-856e-2dc8b6d5db26"
  , "84ea6913-0106-4ac1-9b84-ba621fc4b345"
  , "eb7a5343-9761-42b0-8258-4b658a36138e"
  , "64661121-b968-4122-89a3-de5be06f88a8"
  , "97cba1d1-528a-417a-86da-bb950d8480cb"
  , "af308922-76d8-47f4-ae97-0f2943b8952d"
  , "6dfb66ca-16c0-4029-bcde-2727f27832a0"
  , "66f9f38c-1f15-4e4e-a852-3be361a4a850"
  , "4ba8818f-c57c-4164-8fef-d38c5bf20da8"
  , "bad4a978-d1a1-40af-a5f2-e0d93888e423"
  , "3699f74a-f216-4c64-b8b2-9beb5c67f5ae"
  , "3c1e5beb-45be-4ead-b42a-4aca4927340d"
  , "5f1e6582-b8ee-44ac-b0db-2e93548b7a38"
  , "c7e39c12-5f9a-402c-abf8-9ee3b7726638"
  , "65407ef2-34a8-4a3c-b638-809125f4a4b4"
  , "542ffd56-a2b3-4548-a308-0946fc748f0b"
  , "cfeef25b-d9f7-4c1f-a9ca-6705df94b70d"
  , "181a9c36-e45a-4b48-bf7a-8a7c17208a40"
  , "ca4df7e6-4693-4854-87a9-689ccb5642be"
  , "1d0ade67-4063-4cc3-aa91-72339432a531"
  , "8e9dcc80-80a2-43ea-969d-53a7e278a022"
  , "73bc56a1-7d1d-41f4-9254-42a1656c7558"
  , "92cdb456-85a1-4fa0-8603-35b9da3a7361"
  , "be181705-4382-41dd-96a5-a61f05cd1059"
  , "fb2c419c-ea3a-4691-a45a-bcd875154f1d"
  , "bbe1a0d3-5615-47df-81ca-b52e066f01e1"
  , "c45f6fcd-b032-4039-9c23-0185f52f5722"
  , "8c250ca1-3a13-401d-8907-49db8f8fd1b1"
  , "958f7a5c-0938-414a-a280-daed6227abeb"
  , "b1bf5322-4729-497e-a227-7ca40d3c3360"
  , "0636f9aa-c831-4882-8af5-a63c0081f683"
  , "3e79509a-c7ce-45a3-8e2a-87d99711612f"
  ]

instance (ToSample a) => ToSample (IdAnd a) where
  toSamples _ =
      zipWith idAndify uuidList $ toSamples Proxy
    where
      idAndify uuid (desc, a) = (desc, IdAnd (uuid,a))

instance (ToSample a) => ToSample (RefMap a) where
  toSamples _ = singleSample .
      RefMap . Map.fromList . zipWith idAndify uuidList $ toSamples Proxy
    where
      idAndify uuid (_, a) = (uuid, IdAnd (uuid,a))

#endif


