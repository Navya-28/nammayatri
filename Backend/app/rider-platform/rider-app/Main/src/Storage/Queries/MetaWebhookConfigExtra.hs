{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | No hand-written extra queries needed today — this file exists solely to
-- satisfy EXTRA_QUERY_FILE, which routes fromTType'/toTType' generation
-- through the split OrphanInstances file (needed for botConfig's JSONB
-- decode to get correctly unwrapped with fromMaybeM, mirroring
-- RiderPreferencesExtra.hs).
module Storage.Queries.MetaWebhookConfigExtra where

-- Instance-only import: pulls the FromTType'/ToTType' instances into the
-- build; nothing here is referenced by name, so plain `import X` would trip
-- -Wunused-imports under -Werror.
import Storage.Queries.OrphanInstances.MetaWebhookConfig ()
