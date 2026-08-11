{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.FrfsFleetOperator where

import qualified "this" API.Types.UI.FRFSFleetOperator
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified "gtfs-data-server" Lib.GtfsDataServer.Types
import Servant
import Servant.Client

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorQueryRow :<|> PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction))

type PostFrfsFleetOperatorQueryRow =
  ( "queryRow" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> MandatoryQueryParam
           "table"
           Lib.GtfsDataServer.Types.NandiTable
      :> ReqBody ('[JSON]) Lib.GtfsDataServer.Types.QueryBody
      :> Post ('[JSON]) [Lib.GtfsDataServer.Types.NandiRow]
  )

type PostFrfsFleetOperatorCurrentOperation =
  ( "currentOperation" :> ReqBody ('[JSON]) API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp
  )

type PostFrfsFleetOperatorTripAction =
  ( "tripAction" :> ReqBody ('[JSON]) API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp
  )

data FrfsFleetOperatorAPIs = FrfsFleetOperatorAPIs
  { postFrfsFleetOperatorQueryRow :: (BecknV2.OnDemand.Enums.VehicleCategory -> Lib.GtfsDataServer.Types.NandiTable -> Lib.GtfsDataServer.Types.QueryBody -> EulerHS.Types.EulerClient [Lib.GtfsDataServer.Types.NandiRow]),
    postFrfsFleetOperatorCurrentOperation :: (API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp),
    postFrfsFleetOperatorTripAction :: (API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
  }

mkFrfsFleetOperatorAPIs :: (Client EulerHS.Types.EulerClient API -> FrfsFleetOperatorAPIs)
mkFrfsFleetOperatorAPIs frfsFleetOperatorClient = (FrfsFleetOperatorAPIs {..})
  where
    postFrfsFleetOperatorQueryRow :<|> postFrfsFleetOperatorCurrentOperation :<|> postFrfsFleetOperatorTripAction = frfsFleetOperatorClient

data FrfsFleetOperatorUserActionType
  = POST_FRFS_FLEET_OPERATOR_QUERY_ROW
  | POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION
  | POST_FRFS_FLEET_OPERATOR_TRIP_ACTION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''FrfsFleetOperatorUserActionType)])
