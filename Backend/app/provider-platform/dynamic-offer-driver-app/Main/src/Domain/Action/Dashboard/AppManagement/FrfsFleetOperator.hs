module Domain.Action.Dashboard.AppManagement.FrfsFleetOperator
  ( postFrfsFleetOperatorQueryRow,
    postFrfsFleetOperatorCurrentOperation,
    postFrfsFleetOperatorTripAction,
  )
where

import qualified API.Types.UI.FRFSFleetOperator
import qualified BecknV2.OnDemand.Enums
import qualified Domain.Action.UI.FRFSFleetOperator as UIFRFSFleetOperator
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (throwError)
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import Lib.GtfsDataServer.Types
import SharedLogic.IntegratedBPPConfig (findFirstIbppConfigByCityAndVehicle, getGimsBaseUrl)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error (GenericError (InvalidRequest))

postFrfsFleetOperatorQueryRow ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  BecknV2.OnDemand.Enums.VehicleCategory ->
  NandiTable ->
  QueryBody ->
  Flow [NandiRow]
postFrfsFleetOperatorQueryRow merchantShortId opCity vehicleCategory table body = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  ibppConfig <- findFirstIbppConfigByCityAndVehicle merchantOpCityId (show vehicleCategory)
  baseUrl <- getGimsBaseUrl ibppConfig
  let gtfsId = DIBC.feedKey ibppConfig
      allowedTables =
        [ RouteInternal,
          RoutePointInternal,
          BusScheduleInternal,
          BusScheduleTripInternal,
          BusScheduleTripDetailInternal,
          BusScheduleTripFlexiInternal,
          ServiceTypeInternal,
          StopInternal,
          DesignationsInternal,
          EntitiesInternal,
          VehiclesInternal,
          WaybillDeviceInternal,
          FleetEtmMappingInternal,
          FleetObuMappingInternal,
          WaybillsInternal,
          BusShiftTypeInternal,
          BusScheduleTypeInternal
        ]
  unless (table `elem` allowedTables) $
    throwError $ InvalidRequest $ "Table " <> nandiTableToText table <> " is not accessible via the dashboard API"
  NandiFlow.operatorQueryRows baseUrl gtfsId table body

postFrfsFleetOperatorCurrentOperation ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq ->
  Flow API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp
postFrfsFleetOperatorCurrentOperation merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  UIFRFSFleetOperator.postFrfsFleetOperatorCurrentOperation (Nothing, merchant.id, merchantOpCityId) req

postFrfsFleetOperatorTripAction ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq ->
  Flow API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp
postFrfsFleetOperatorTripAction merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  UIFRFSFleetOperator.postFrfsFleetOperatorTripAction (Nothing, merchant.id, merchantOpCityId) req
