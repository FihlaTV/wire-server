{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Federator.API where

import Brig.Types.Client.Prekey
import Brig.Types.Test.Arbitrary ()
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Handle (Handle (..))
import Data.Id (ConvId, UserId)
import Data.Qualified
import Federator.Util
import qualified Galley.Types.Conversations.Roles as Galley.Roles
import qualified Galley.Types.QualifiedEvent as Galley
import Imports
import Servant.API
import Servant.API.Generic
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, arbitrary)

data API route
  = API
      { _gapiSearch ::
          route
            :- "i"
            :> "search"
            -- QUESTION: what exactly should the query be? text + domain?
            :> QueryParam' [Required, Strict] "q" (Qualified Handle)
            :> Get '[JSON] FUser,
        _gapiPrekeys ::
          route
            :- "i"
            :> "users"
            :> Capture "fqu" (Qualified UserId)
            :> "prekeys"
            :> Get '[JSON] PrekeyBundle,
        _gapiJoinConversation ::
          route
            :- "i"
            :> "conversations"
            :> Capture "cnv" (Qualified ConvId)
            :> "join"
            :> Post '[JSON] (Galley.QualifiedEvent Galley.MemberJoin)
      }
  deriving (Generic)

-- curl http://localhost:8097/i/search?q=wef@a.com; curl http://localhost:8097/i/users/`uuid`@example.com/prekeys

----------------------------------------------------------------------
-- TODO: add roundtrip tests for *HttpApiData, *JSON, ...
--
-- TODO: the client ids in the 'PrekeyBundle' aren't really needed here.  do we want to make a
-- new type for that, then?

data FUser
  = FUser
      { _fuGlobalHandle :: !(Qualified Handle),
        _fuFQU :: !(Qualified UserId)
      }
  deriving (Eq, Show, Generic)

deriveJSON (wireJsonOptions "_fu") ''FUser

instance Arbitrary FUser where
  arbitrary = FUser <$> arbitrary <*> arbitrary

----------------------------------------------------------------------
-- ORPHANS

instance Arbitrary PrekeyBundle where
  arbitrary = PrekeyBundle <$> arbitrary <*> arbitrary

instance Arbitrary ClientPrekey where
  arbitrary = ClientPrekey <$> arbitrary <*> arbitrary

instance Arbitrary (Galley.QualifiedEvent Galley.MemberJoin) where
  arbitrary =
    Galley.QualifiedEvent
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Galley.MemberJoin where
  arbitrary = Galley.QuMemberJoin . Galley.QualifiedSimpleMembers <$> arbitrary

instance Arbitrary Galley.QualifiedSimpleMember where
  arbitrary =
    Galley.QualifiedSimpleMember
      <$> arbitrary
      <*> arbitrary

instance Arbitrary Galley.Roles.RoleName where
  arbitrary =
    QC.oneof
      [ QC.elements Galley.Roles.wireConvRoleNames,
        arbitrary `QC.suchThatMap` \txt ->
          case Aeson.fromJSON (Aeson.String txt) of
            Aeson.Success s -> Just s
            Aeson.Error _ -> Nothing
      ]
