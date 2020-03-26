module Brig.Team.Util where -- TODO: remove this module and move contents to Brig.IO.Intra?

import Brig.API.Error
import Brig.App
import qualified Brig.IO.Intra as Intra
import Control.Error
import Control.Lens
import Data.Id
import qualified Data.Set as Set
import Galley.Types.Teams
import Imports

ensurePermissions :: UserId -> TeamId -> [Perm] -> ExceptT Error AppIO ()
ensurePermissions u t perms = do
  m <- lift $ Intra.getTeamMember u t
  unless (check m) $
    throwStd insufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just m) = and $ hasPermission m <$> perms
    check Nothing = False

-- | Privilege escalation detection (make sure no `RoleMember` user creates a `RoleOwner`).
--
-- There is some code duplication with 'Galley.API.Teams.ensureNotElevated'.
ensurePermissionToAddUser :: UserId -> TeamId -> Permissions -> ExceptT Error AppIO ()
ensurePermissionToAddUser u t inviteePerms = do
  minviter <- lift $ Intra.getTeamMember u t
  unless (check minviter) $
    throwStd insufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just inviter) =
      hasPermission inviter AddTeamMember
        && and (mayGrantPermission inviter <$> Set.toList (inviteePerms ^. self))
    check Nothing = False
