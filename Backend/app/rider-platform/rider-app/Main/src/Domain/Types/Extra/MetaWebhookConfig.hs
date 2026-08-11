{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Per-phone-number WhatsApp-bot tuning config, persisted as the @bot_config@
-- jsonb column of @meta_config@. Field-for-field copy of the old
-- Dhall-sourced @Environment.MetaBotCfg@, with an Aeson codec instead of
-- @FromDhall@ (this JSON only ever round-trips through our own DB column,
-- never the Meta wire format, so no key remapping is needed here).
module Domain.Types.Extra.MetaWebhookConfig where

import Data.Aeson
import Kernel.Prelude

data MetaBotCfg = MetaBotCfg
  { merchantLabel :: Text,
    rideMode :: Text,
    flexiBaseFare :: Maybe Double,
    flexiPerKm :: Maybe Double,
    flexiServiceArea :: Maybe Text,
    flexiServiceRadiusKm :: Maybe Double,
    flexiRentalDistanceM :: Int,
    flexiRentalDurationS :: Int,
    flexiIntroVideoUrl :: Maybe Text,
    flexiSupportPhone :: Maybe Text,
    nyTrackingUrl :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
