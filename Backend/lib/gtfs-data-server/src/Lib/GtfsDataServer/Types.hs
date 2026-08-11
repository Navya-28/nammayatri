{-# LANGUAGE DuplicateRecordFields #-}

module Lib.GtfsDataServer.Types where

import qualified BecknV2.FRFS.Enums
import Control.Lens ((?~))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.OpenApi (OpenApiType (..), ToParamSchema (..), enum_, toParamSchema, type_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Kernel.External.Maps (HasCoordinates (..))
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.HideSecrets (HideSecrets (..))
import Kernel.Types.Time (Seconds)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

sanitizeJsonQuotes :: Text -> Text
sanitizeJsonQuotes = T.replace "'" "\""

data Gate = Gate
  { gateName :: String,
    stopCode :: String,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance HasCoordinates Gate

data RouteStopMappingInMemoryServer = RouteStopMappingInMemoryServer
  { estimatedTravelTimeFromPreviousStop :: Maybe Seconds,
    providerCode :: Text,
    routeCode :: Text,
    sequenceNum :: Int,
    stopCode :: Text,
    stopName :: Text,
    stopPoint :: LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    hindiName :: Maybe Text,
    regionalName :: Maybe Text,
    parentStopCode :: Maybe Text,
    gates :: Maybe [Gate]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data RouteInfoNandi = RouteInfoNandi
  { id :: Text,
    shortName :: Maybe Text,
    longName :: Maybe Text,
    mode :: BecknV2.FRFS.Enums.VehicleCategory,
    agencyName :: Maybe Text,
    tripCount :: Maybe Int,
    startPoint :: LatLong,
    endPoint :: LatLong,
    stopCount :: Maybe Int,
    serviceTierType :: Maybe BecknV2.FRFS.Enums.ServiceTierType,
    encodedPolyline :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- | Per-stop ETA entry returned by GIMS' `bus-trip-schedule` endpoint. GIMS sends the arrival as a
-- Unix epoch under snake_case keys, so JSON is hand-written (mirrors the rider app's `BusStopETA`).
utcToIST :: UTCTime -> UTCTime
utcToIST = addUTCTime 19800

data BusStopETA = BusStopETA
  { stopCode :: Text,
    stopName :: Maybe Text,
    arrivalTime :: UTCTime,
    arrivalTimeUnix :: Integer,
    etaSeconds :: Maybe Integer
  }
  deriving (Generic, Show, Eq)

instance FromJSON BusStopETA where
  parseJSON = withObject "BusStopETA" $ \v -> do
    stopCode <- v .: "stop_id"
    arrivalTimeUnix <- v .: "arrival_time"
    etaSeconds <- v .:? "eta_seconds"
    stopName <- v .:? "stop_name"
    let arrivalTime = utcToIST $ posixSecondsToUTCTime $ realToFrac arrivalTimeUnix
    return $ BusStopETA {..}

instance ToJSON BusStopETA where
  toJSON BusStopETA {..} =
    object
      [ "stop_id" .= stopCode,
        "arrival_time" .= (floor (realToFrac (utcTimeToPOSIXSeconds arrivalTime) :: Double) :: Integer),
        "eta_seconds" .= etaSeconds,
        "stop_name" .= stopName,
        "arrival_time_unix" .= arrivalTimeUnix
      ]

data BusScheduleDetail = BusScheduleDetail
  { eta :: [BusStopETA],
    vehicle_no :: Text,
    service_tier :: BecknV2.FRFS.Enums.ServiceTierType,
    trip_number :: Maybe Int,
    waybill_no :: Maybe Text,
    is_active_trip :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show)

type BusScheduleDetails = [BusScheduleDetail]

data ExtraInfo = ExtraInfo
  { fareStageNumber :: Maybe Text,
    providerStopCode :: Maybe Text,
    isStageStop :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data StopInfo = StopInfo
  { stopId :: Text,
    stopCode :: Text,
    stopName :: Text,
    sequenceNum :: Int,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic)

instance FromJSON StopInfo where
  parseJSON = withObject "StopInfo" $ \v ->
    StopInfo
      <$> v .: "stopId"
      <*> v .: "stopCode"
      <*> v .: "stopName"
      <*> v .: "sequence"
      <*> v .: "lat"
      <*> v .: "lon"

instance ToJSON StopInfo where
  toJSON (StopInfo sId sCode sName seqNum lat' lon') =
    object
      [ "stopId" .= sId,
        "stopCode" .= sCode,
        "stopName" .= sName,
        "sequence" .= seqNum,
        "lat" .= lat',
        "lon" .= lon'
      ]

data StopSchedule = StopSchedule
  { stopCode :: Text,
    arrivalTime :: Int,
    departureTime :: Int,
    sequenceNum :: Int
  }
  deriving (Show, Generic)

instance FromJSON StopSchedule where
  parseJSON = withObject "StopSchedule" $ \v ->
    StopSchedule
      <$> v .: "stopCode"
      <*> v .: "arrivalTime"
      <*> v .: "departureTime"
      <*> v .: "sequence"

instance ToJSON StopSchedule where
  toJSON (StopSchedule sCode arrTime depTime seqNum) =
    object
      [ "stopCode" .= sCode,
        "arrivalTime" .= arrTime,
        "departureTime" .= depTime,
        "sequence" .= seqNum
      ]

data TripStopDetail = TripStopDetail
  { stopId :: Text,
    stopCode :: Text,
    stopName :: Maybe Text,
    platformCode :: Maybe Text,
    lat :: Double,
    lon :: Double,
    scheduledArrival :: Int,
    scheduledDeparture :: Int,
    extraInfo :: Maybe ExtraInfo,
    stopPosition :: Int
  }
  deriving (Generic, Show)

instance FromJSON TripStopDetail where
  parseJSON = withObject "TripStopDetail" $ \obj -> do
    headsignParser <- do
      mHeadsignText <- obj .:? "headsign"
      case mHeadsignText of
        Nothing -> pure Nothing
        Just headsignText -> do
          let sanitized = sanitizeJsonQuotes headsignText
          case eitherDecodeStrict (TE.encodeUtf8 sanitized) of
            Right (Object headsignObj) -> do
              ei <- parseJSON (Object headsignObj)
              pure (Just ei)
            Right (String jsonString) -> do
              case eitherDecodeStrict (TE.encodeUtf8 (sanitizeJsonQuotes jsonString)) of
                Right (Object headsignObj) -> do
                  ei <- parseJSON (Object headsignObj)
                  pure (Just ei)
                _ -> pure (Just (ExtraInfo (Just headsignText) Nothing Nothing))
            _ -> pure (Just (ExtraInfo (Just headsignText) Nothing Nothing))
    TripStopDetail
      <$> obj .: "stopId"
      <*> obj .: "stopCode"
      <*> obj .:? "stopName"
      <*> obj .:? "platformCode"
      <*> obj .: "lat"
      <*> obj .: "lon"
      <*> obj .: "scheduledArrival"
      <*> obj .: "scheduledDeparture"
      <*> pure headsignParser
      <*> obj .: "stopPosition"

instance ToJSON TripStopDetail where
  toJSON (TripStopDetail sId sCode sName pCode lat' lon' schedArr schedDep ei stopPos) =
    object
      [ "stopId" .= sId,
        "stopCode" .= sCode,
        "stopName" .= sName,
        "platformCode" .= pCode,
        "lat" .= lat',
        "lon" .= lon',
        "scheduledArrival" .= schedArr,
        "scheduledDeparture" .= schedDep,
        "extraInfo" .= ei,
        "stopPosition" .= stopPos
      ]

data TripDetails = TripDetails
  { tripId :: Text,
    stops :: [TripStopDetail]
  }
  deriving (Generic, Show)

instance FromJSON TripDetails where
  parseJSON = withObject "TripDetails" $ \obj ->
    TripDetails
      <$> obj .: "tripId"
      <*> obj .: "stops"

instance ToJSON TripDetails where
  toJSON (TripDetails tid ss) =
    object ["tripId" .= tid, "stops" .= ss]

-- | Single stop-code lookup response (GIMS "stop-code" endpoint).
newtype StopCodeResponse = StopCodeResponse
  { stop_code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GimsTripAction
  = GimsTripActionStart
  | GimsTripActionEnd
  | GimsTripActionReset
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON GimsTripAction where
  toJSON GimsTripActionStart = toJSON ("start" :: Text)
  toJSON GimsTripActionEnd = toJSON ("end" :: Text)
  toJSON GimsTripActionReset = toJSON ("reset" :: Text)

instance FromJSON GimsTripAction where
  parseJSON = withText "GimsTripAction" $ \case
    "start" -> pure GimsTripActionStart
    "end" -> pure GimsTripActionEnd
    "reset" -> pure GimsTripActionReset
    v -> fail $ "Unknown GimsTripAction: " <> T.unpack v

data GimsTripActionReq = GimsTripActionReq
  { action :: GimsTripAction,
    tripNumber :: Maybe Int,
    timestamp :: Maybe Int64,
    gimsConductorId :: Maybe Text,
    gimsDriverId :: Maybe Text,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON GimsTripActionReq where
  parseJSON = withObject "GimsTripActionReq" $ \o ->
    GimsTripActionReq
      <$> o .: "action"
      <*> o .:? "trip_number"
      <*> o .:? "timestamp"
      <*> o .:? "conductor_token"
      <*> o .:? "driver_token"
      <*> o .:? "vehicle_number"

instance ToJSON GimsTripActionReq where
  toJSON GimsTripActionReq {..} =
    object
      [ "action" .= action,
        "trip_number" .= tripNumber,
        "timestamp" .= timestamp,
        "conductor_token" .= gimsConductorId,
        "driver_token" .= gimsDriverId,
        "vehicle_number" .= vehicleNumber
      ]

instance HideSecrets GimsTripActionReq where
  hideSecrets GimsTripActionReq {..} =
    GimsTripActionReq
      { action = action,
        tripNumber = tripNumber,
        timestamp = timestamp,
        gimsConductorId = "***" <$ gimsConductorId,
        gimsDriverId = "***" <$ gimsDriverId,
        vehicleNumber = vehicleNumber
      }

data GimsCurrentOperationResp = GimsCurrentOperationResp
  { waybill_no :: Text,
    number_of_trips :: Int,
    trip_numbers :: Maybe [Int]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GimsCurrentTripDetailsReq = GimsCurrentTripDetailsReq
  { previousTripNumber :: Int,
    gimsConductorId :: Maybe Text,
    gimsDriverId :: Maybe Text,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON GimsCurrentTripDetailsReq where
  parseJSON = withObject "GimsCurrentTripDetailsReq" $ \o ->
    GimsCurrentTripDetailsReq
      <$> o .: "previous_trip_number"
      <*> o .:? "conductor_token"
      <*> o .:? "driver_token"
      <*> o .:? "vehicle_number"

instance ToJSON GimsCurrentTripDetailsReq where
  toJSON GimsCurrentTripDetailsReq {..} =
    object
      [ "previous_trip_number" .= previousTripNumber,
        "conductor_token" .= gimsConductorId,
        "driver_token" .= gimsDriverId,
        "vehicle_number" .= vehicleNumber
      ]

data GimsTripInfo = GimsTripInfo
  { trip_number :: Int,
    route_id :: Text,
    route_number :: Text,
    route_name :: Text,
    is_active_trip :: Bool,
    duty_date :: Maybe Text,
    start_time :: Maybe Text,
    end_time :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

instance FromJSON GimsTripInfo where
  parseJSON = withObject "GimsTripInfo" $ \v -> do
    trip_number <- v .: "trip_number"
    route_id <- v .: "route_id"
    route_number <- fromMaybe route_id <$> (v .:? "route_number")
    route_name <- fromMaybe "" <$> (v .:? "route_name")
    is_active_trip <- v .: "is_active_trip"
    duty_date <- v .:? "duty_date"
    start_time <- v .:? "start_time"
    end_time <- v .:? "end_time"
    return GimsTripInfo {..}

data GimsCurrentTripDetailsResp = GimsCurrentTripDetailsResp
  { waybillNo :: Text,
    vehicleNumber :: Text,
    gimsConductorId :: Maybe Text,
    gimsDriverId :: Maybe Text,
    history :: [GimsTripInfo],
    current :: Maybe GimsTripInfo,
    upcoming :: [GimsTripInfo]
  }
  deriving (Generic, Show)

instance FromJSON GimsCurrentTripDetailsResp where
  parseJSON = withObject "GimsCurrentTripDetailsResp" $ \o ->
    GimsCurrentTripDetailsResp
      <$> o .: "waybill_no"
      <*> o .: "vehicle_number"
      <*> o .:? "conductor_token"
      <*> o .:? "driver_token"
      <*> o .: "history"
      <*> o .:? "current"
      <*> o .: "upcoming"

instance ToJSON GimsCurrentTripDetailsResp where
  toJSON GimsCurrentTripDetailsResp {..} =
    object
      [ "waybill_no" .= waybillNo,
        "vehicle_number" .= vehicleNumber,
        "conductor_token" .= gimsConductorId,
        "driver_token" .= gimsDriverId,
        "history" .= history,
        "current" .= current,
        "upcoming" .= upcoming
      ]

data GimsOperationAnchor = GimsOperationAnchor
  { gimsConductorId :: Maybe Text,
    gimsDriverId :: Maybe Text,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON GimsOperationAnchor where
  parseJSON = withObject "GimsOperationAnchor" $ \o ->
    GimsOperationAnchor
      <$> o .:? "conductor_token"
      <*> o .:? "driver_token"
      <*> o .:? "vehicle_number"

instance ToJSON GimsOperationAnchor where
  toJSON GimsOperationAnchor {..} =
    object
      [ "conductor_token" .= gimsConductorId,
        "driver_token" .= gimsDriverId,
        "vehicle_number" .= vehicleNumber
      ]

instance HideSecrets GimsOperationAnchor where
  hideSecrets GimsOperationAnchor {..} =
    GimsOperationAnchor
      { gimsConductorId = "***" <$ gimsConductorId,
        gimsDriverId = "***" <$ gimsDriverId,
        vehicleNumber = vehicleNumber
      }

-- | Verify conductor badge token against device serial number (GIMS "verify" endpoint).
data GimsVerifyReq = GimsVerifyReq
  { operator_badge_token :: Text,
    device_serial_number :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance HideSecrets GimsVerifyReq where
  hideSecrets = identity

newtype GimsVerifyResp = GimsVerifyResp
  { verified :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- | Employee login request for conductor GIMS auth (driver-app only).
-- `auth_type` selects which identifier is sent: "Email" -> email_hash (hashed,
-- PII), "EmployeeId" -> employee_id (plain; it is a username, not a secret).
-- Only the matching identifier is populated; omitNothingFields keeps the unused one
-- off the wire so the existing Email contract is unchanged. The password is always
-- pre-hashed: SHA256(hashSalt <> value).
data GimsEmployeeLoginReq = GimsEmployeeLoginReq
  { auth_type :: Maybe Text,
    email_hash :: Maybe Text,
    employee_id :: Maybe Text,
    password_hash :: Text
  }
  deriving (Generic, Show)

instance ToJSON GimsEmployeeLoginReq where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON GimsEmployeeLoginReq where
  parseJSON = genericParseJSON defaultOptions

instance HideSecrets GimsEmployeeLoginReq where
  hideSecrets GimsEmployeeLoginReq {..} =
    GimsEmployeeLoginReq
      { auth_type = auth_type,
        email_hash = "***" <$ email_hash,
        employee_id = employee_id,
        password_hash = "***"
      }

-- | Roles GIMS recognises for an employee. Wire form is lowercase
-- (\"driver\" / \"conductor\"); parser is case-insensitive. Unknown values
-- fail the whole login response — keep this in sync with the Rust enum.
data GimsEmployeeRole = GimsDriver | GimsConductor
  deriving (Generic, Show, Eq)

instance FromJSON GimsEmployeeRole where
  parseJSON = withText "GimsEmployeeRole" $ \t -> case T.toLower t of
    "driver" -> pure GimsDriver
    "conductor" -> pure GimsConductor
    other -> fail $ "Unknown GIMS employee role: " <> T.unpack other

instance ToJSON GimsEmployeeRole where
  toJSON GimsDriver = String "driver"
  toJSON GimsConductor = String "conductor"

-- | Response from the employee login endpoint (driver-app only).
data GimsEmployeeLoginResp = GimsEmployeeLoginResp
  { verified :: Bool,
    token :: Maybe Text,
    role :: Maybe GimsEmployeeRole
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- ─── Nandi CRUD query types ───────────────────────────────────────────────────

data NandiTable
  = RouteInternal
  | RoutePointInternal
  | BusScheduleInternal
  | BusScheduleTripInternal
  | BusScheduleTripDetailInternal
  | BusScheduleTripFlexiInternal
  | ServiceTypeInternal
  | StopInternal
  | DesignationsInternal
  | EmployeesInternal
  | EntitiesInternal
  | VehiclesInternal
  | WaybillDeviceInternal
  | FleetEtmMappingInternal
  | FleetObuMappingInternal
  | WaybillsInternal
  | BusShiftTypeInternal
  | BusScheduleTypeInternal
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToSchema)

nandiTableToText :: NandiTable -> Text
nandiTableToText RouteInternal = "route_internal"
nandiTableToText RoutePointInternal = "route_point_internal"
nandiTableToText BusScheduleInternal = "bus_schedule_internal"
nandiTableToText BusScheduleTripInternal = "bus_schedule_trip_internal"
nandiTableToText BusScheduleTripDetailInternal = "bus_schedule_trip_detail_internal"
nandiTableToText BusScheduleTripFlexiInternal = "bus_schedule_trip_flexi_internal"
nandiTableToText ServiceTypeInternal = "service_type_internal"
nandiTableToText StopInternal = "stop_internal"
nandiTableToText DesignationsInternal = "designations_internal"
nandiTableToText EmployeesInternal = "employees_internal"
nandiTableToText EntitiesInternal = "entities_internal"
nandiTableToText VehiclesInternal = "vehicles_internal"
nandiTableToText WaybillDeviceInternal = "waybill_device_internal"
nandiTableToText FleetEtmMappingInternal = "fleet_etm_mapping_internal"
nandiTableToText FleetObuMappingInternal = "fleet_obu_mapping_internal"
nandiTableToText WaybillsInternal = "waybills_internal"
nandiTableToText BusShiftTypeInternal = "bus_shift_type_internal"
nandiTableToText BusScheduleTypeInternal = "bus_schedule_type_internal"

nandiTableFromText :: Text -> Either Text NandiTable
nandiTableFromText "route_internal" = Right RouteInternal
nandiTableFromText "route_point_internal" = Right RoutePointInternal
nandiTableFromText "bus_schedule_internal" = Right BusScheduleInternal
nandiTableFromText "bus_schedule_trip_internal" = Right BusScheduleTripInternal
nandiTableFromText "bus_schedule_trip_detail_internal" = Right BusScheduleTripDetailInternal
nandiTableFromText "bus_schedule_trip_flexi_internal" = Right BusScheduleTripFlexiInternal
nandiTableFromText "service_type_internal" = Right ServiceTypeInternal
nandiTableFromText "stop_internal" = Right StopInternal
nandiTableFromText "designations_internal" = Right DesignationsInternal
nandiTableFromText "employees_internal" = Right EmployeesInternal
nandiTableFromText "entities_internal" = Right EntitiesInternal
nandiTableFromText "vehicles_internal" = Right VehiclesInternal
nandiTableFromText "waybill_device_internal" = Right WaybillDeviceInternal
nandiTableFromText "fleet_etm_mapping_internal" = Right FleetEtmMappingInternal
nandiTableFromText "fleet_obu_mapping_internal" = Right FleetObuMappingInternal
nandiTableFromText "waybills_internal" = Right WaybillsInternal
nandiTableFromText "bus_shift_type_internal" = Right BusShiftTypeInternal
nandiTableFromText "bus_schedule_type_internal" = Right BusScheduleTypeInternal
nandiTableFromText t = Left $ "Unknown NandiTable: " <> t

instance ToJSON NandiTable where
  toJSON = toJSON . nandiTableToText

instance FromJSON NandiTable where
  parseJSON = withText "NandiTable" $ \t ->
    either (fail . T.unpack) pure (nandiTableFromText t)

instance ToHttpApiData NandiTable where
  toQueryParam = nandiTableToText

instance FromHttpApiData NandiTable where
  parseQueryParam = nandiTableFromText

instance ToParamSchema NandiTable where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ map (toJSON . nandiTableToText) [minBound .. maxBound]

data WaybillStatus
  = WaybillOnline
  | WaybillUpcoming
  | WaybillNew
  | WaybillProcessed
  | WaybillAudited
  | WaybillClosed
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToSchema)

instance ToJSON WaybillStatus where
  toJSON WaybillOnline = toJSON ("online" :: Text)
  toJSON WaybillUpcoming = toJSON ("upcoming" :: Text)
  toJSON WaybillNew = toJSON ("new" :: Text)
  toJSON WaybillProcessed = toJSON ("processed" :: Text)
  toJSON WaybillAudited = toJSON ("audited" :: Text)
  toJSON WaybillClosed = toJSON ("closed" :: Text)

instance FromJSON WaybillStatus where
  parseJSON = withText "WaybillStatus" $ \v ->
    case T.toLower v of
      "online" -> pure WaybillOnline
      "upcoming" -> pure WaybillUpcoming
      "new" -> pure WaybillNew
      "processed" -> pure WaybillProcessed
      "audited" -> pure WaybillAudited
      "closed" -> pure WaybillClosed
      _ -> fail $ "Unknown WaybillStatus: " <> T.unpack v

data BreakType = NoBreak | FoodBreak | TeaBreak
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToSchema)

instance ToJSON BreakType where
  toJSON NoBreak = toJSON ("no-break" :: Text)
  toJSON FoodBreak = toJSON ("food-break" :: Text)
  toJSON TeaBreak = toJSON ("tea-break" :: Text)

instance FromJSON BreakType where
  parseJSON = withText "BreakType" $ \v ->
    case T.toLower v of
      "no-break" -> pure NoBreak
      "food-break" -> pure FoodBreak
      "tea-break" -> pure TeaBreak
      _ -> fail $ "Unknown BreakType: " <> T.unpack v

data TripType = CutTrip | RegularTrip | DeadTrip
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToSchema)

instance ToJSON TripType where
  toJSON CutTrip = toJSON ("cut-trip" :: Text)
  toJSON RegularTrip = toJSON ("regular-trip" :: Text)
  toJSON DeadTrip = toJSON ("dead-trip" :: Text)

instance FromJSON TripType where
  parseJSON = withText "TripType" $ \v ->
    case T.toLower v of
      "cut-trip" -> pure CutTrip
      "regular-trip" -> pure RegularTrip
      "dead-trip" -> pure DeadTrip
      _ -> fail $ "Unknown TripType: " <> T.unpack v

data NandiRouteRow = NandiRouteRow
  { route_id :: Value,
    created_at :: Maybe UTCTime,
    description :: Maybe Text,
    route_direction :: Maybe Text,
    route_group :: Maybe Text,
    route_name :: Maybe Text,
    route_number :: Maybe Text,
    route_string :: Maybe Text,
    route_type_id :: Value,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    via :: Maybe Text,
    bus_service_type_id :: Value,
    end_point_id :: Value,
    start_point_id :: Value,
    route_distance :: Maybe Double,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiRoutePointRow = NandiRoutePointRow
  { route_points_id :: Value,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    fare_stage :: Maybe Text,
    point_status :: Maybe Text,
    route_order :: Int,
    stage_no :: Maybe Int,
    sub_stage :: Maybe Text,
    travel_distance :: Maybe Int,
    travel_time :: Maybe Text,
    updated_at :: Maybe UTCTime,
    bus_stop_id :: Value,
    route_id :: Value,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleRow = NandiBusScheduleRow
  { schedule_id :: Value,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    effective_from :: Maybe UTCTime,
    effective_till :: Maybe UTCTime,
    route_code :: Maybe Text,
    schedule_number :: Maybe Text,
    service_code :: Maybe Text,
    service_type_code :: Maybe Text,
    schedule_type_code :: Maybe Text,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    entity_id :: Value,
    route_id :: Value,
    service_type_id :: Value,
    schedule_type_id :: Value,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleTripRow = NandiBusScheduleTripRow
  { schedule_trip_id :: Value,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    effective_end_date :: Maybe UTCTime,
    effective_start_date :: Maybe UTCTime,
    no_trip :: Int,
    schedule_number_name :: Maybe Text,
    start_time :: Maybe Text,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    calendar_id :: Value,
    schedule_id :: Value,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleTripDetailRow = NandiBusScheduleTripDetailRow
  { schedule_trip_detail_id :: Value,
    break_time :: Maybe Text,
    break_type :: Maybe BreakType,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    distance :: Double,
    end_time :: Maybe Text,
    org_name :: Maybe Text,
    running_time :: Maybe Text,
    schedule_number :: Maybe Text,
    shift_day_name :: Maybe Text,
    shift_type_name :: Maybe Text,
    start_time :: Maybe Text,
    trip_number :: Int,
    trip_order :: Int,
    trip_type :: Maybe TripType,
    updated_at :: Maybe UTCTime,
    calendar_id :: Value,
    route_number_id :: Value,
    schedule_trip_id :: Value,
    is_active_trip :: Bool,
    trip_end_time :: Maybe Int,
    trip_start_time :: Maybe Int,
    sync_end_time :: Maybe Int,
    sync_start_time :: Maybe Int,
    status :: Maybe Text,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleTripFlexiRow = NandiBusScheduleTripFlexiRow
  { schedule_trip_flexi_id :: Value,
    break_time :: Maybe Text,
    break_type :: Maybe BreakType,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    distance :: Double,
    end_time :: Maybe Text,
    org_name :: Maybe Text,
    running_time :: Maybe Text,
    schedule_number :: Maybe Text,
    shift_day_name :: Maybe Text,
    shift_type_name :: Maybe Text,
    start_time :: Maybe Text,
    trip_number :: Int,
    trip_order :: Int,
    trip_type :: Maybe TripType,
    updated_at :: Maybe UTCTime,
    calendar_id :: Value,
    route_number_id :: Value,
    schedule_trip_id :: Value,
    waybill_id :: Value,
    is_active_trip :: Bool,
    trip_end_time :: Maybe Int,
    trip_start_time :: Maybe Int,
    sync_end_time :: Maybe Int,
    sync_start_time :: Maybe Int,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiServiceTypeRow = NandiServiceTypeRow
  { service_type_id :: Value,
    abbreviation :: Maybe Text,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    service_type_code :: Maybe Text,
    service_type_name :: Maybe Text,
    status :: Maybe Text,
    ticket_footer :: Maybe Text,
    ticket_footer_local_lang :: Maybe Text,
    updated_at :: Maybe UTCTime,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiStopRow = NandiStopRow
  { bus_stop_id :: Value,
    bus_stop_code :: Maybe Text,
    bus_stop_name :: Maybe Text,
    bus_stop_name_local_lang :: Maybe Text,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    description :: Maybe Text,
    fare_stage :: Maybe Text,
    landmark :: Maybe Text,
    latitude_current :: Double,
    longitude_current :: Double,
    route_status :: Maybe Text,
    source :: Maybe Text,
    status :: Maybe Text,
    stop_direction :: Maybe Text,
    stop_group_id :: Maybe Value,
    stop_type_id :: Value,
    sub_stage :: Maybe Text,
    toll_fee :: Maybe Int,
    toll_zone :: Maybe Text,
    updated_at :: Maybe UTCTime,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiDesignationRow = NandiDesignationRow
  { designation_id :: Value,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    designation_name :: Text,
    designation_remark :: Maybe Text,
    designation_status :: Text,
    is_default :: Maybe Int,
    updated_at :: Maybe UTCTime,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiEmployeeRow = NandiEmployeeRow
  { emp_id :: Value,
    address :: Maybe Text,
    basic_amount :: Maybe Double,
    created_at :: Maybe UTCTime,
    da_amount :: Maybe Double,
    dob :: Maybe Day,
    deleted :: Bool,
    driving_license_expiry :: Maybe Text,
    driving_license_number :: Maybe Text,
    email :: Maybe Text,
    email_hash :: Maybe Text,
    password_hash :: Maybe Text,
    father_name :: Maybe Text,
    first_name :: Text,
    gender :: Maybe Text,
    last_name :: Maybe Text,
    mobile_no :: Maybe Text,
    status :: Maybe Text,
    token_no :: Maybe Text,
    updated_at :: Maybe UTCTime,
    week_off :: Maybe Text,
    department_id :: Value,
    designation_id :: Value,
    entity_id :: Value,
    organization_id :: Value,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToSchema, Show)

instance ToJSON NandiEmployeeRow where
  toJSON row =
    case genericToJSON defaultOptions row of
      Object obj ->
        Object $
          foldr
            KM.delete
            obj
            ["email_hash", "password_hash", "email", "mobile_no", "dob", "address", "father_name", "basic_amount", "da_amount"]
      other -> other

data NandiEntityRow = NandiEntityRow
  { entity_id :: Value,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    entity_address :: Maybe Text,
    entity_contact :: Maybe Text,
    entity_email :: Maybe Text,
    entity_name :: Text,
    entity_name_local_lang :: Maybe Text,
    entity_remark :: Maybe Text,
    entity_status :: Text,
    updated_at :: Maybe UTCTime,
    organization_id :: Value,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiVehicleRow = NandiVehicleRow
  { vehicle_id :: Value,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    fleet_no :: Maybe Text,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    vehicle_no :: Maybe Text,
    bus_service_type_id :: Value,
    entity_id :: Value,
    organization_id :: Value,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiWaybillDeviceRow = NandiWaybillDeviceRow
  { waybill_device_id :: Value,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    device_serial_no :: Maybe Text,
    is_audited :: Maybe Bool,
    is_primary :: Maybe Bool,
    is_uploaded :: Maybe Bool,
    updated_at :: Maybe UTCTime,
    waybill_id :: Value,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiFleetEtmMappingRow = NandiFleetEtmMappingRow
  { fleet_etm_mapping_id :: Value,
    vehicle_no :: Text,
    gtfs_id :: Text,
    etm_serial_no :: Text,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime,
    deleted :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiFleetObuMappingRow = NandiFleetObuMappingRow
  { fleet_obu_mapping_id :: Value,
    vehicle_no :: Text,
    gtfs_id :: Text,
    obu_id :: Text,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime,
    deleted :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiWaybillRow = NandiWaybillRow
  { waybill_id :: Value,
    audited_date :: Maybe UTCTime,
    bag_master :: Maybe Text,
    challan_no :: Maybe Int64,
    conductor_name :: Maybe Text,
    conductor_token_no :: Maybe Text,
    created_at :: Maybe UTCTime,
    dc_name :: Maybe Text,
    dc_token_no :: Maybe Text,
    deleted :: Bool,
    driver_name :: Maybe Text,
    driver_token_no :: Maybe Text,
    duty_date :: Maybe Text,
    device_serial_number :: Maybe Text,
    is_flexi :: Bool,
    no_of_device :: Int,
    schedule_id :: Value,
    schedule_no :: Maybe Text,
    schedule_trip_name :: Maybe Text,
    schedule_type :: Maybe Text,
    service_type :: Maybe Text,
    schedule_start_time :: Maybe Text,
    status :: Maybe WaybillStatus,
    updated_at :: Maybe UTCTime,
    vehicle_no :: Maybe Text,
    waybill_no :: Maybe Text,
    entity_id :: Value,
    schedule_trip_id :: Value,
    service_type_id :: Value,
    shift_type_id :: Value,
    tablet_id :: Maybe Text,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiShiftTypeRow = NandiShiftTypeRow
  { shift_type_id :: Value,
    shift_type_code :: Maybe Text,
    description :: Maybe Text,
    gtfs_id :: Text,
    deleted :: Maybe Bool,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiScheduleTypeRow = NandiScheduleTypeRow
  { schedule_type_id :: Value,
    schedule_type_code :: Maybe Text,
    schedule_type_name :: Maybe Text,
    gtfs_id :: Text,
    deleted :: Maybe Bool,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiRow
  = RowRoute NandiRouteRow
  | RowRoutePoint NandiRoutePointRow
  | RowBusSchedule NandiBusScheduleRow
  | RowBusScheduleTrip NandiBusScheduleTripRow
  | RowBusScheduleTripDetail NandiBusScheduleTripDetailRow
  | RowBusScheduleTripFlexi NandiBusScheduleTripFlexiRow
  | RowServiceType NandiServiceTypeRow
  | RowStop NandiStopRow
  | RowDesignation NandiDesignationRow
  | RowEmployee NandiEmployeeRow
  | RowEntity NandiEntityRow
  | RowVehicle NandiVehicleRow
  | RowWaybillDevice NandiWaybillDeviceRow
  | RowFleetEtmMapping NandiFleetEtmMappingRow
  | RowFleetObuMapping NandiFleetObuMappingRow
  | RowWaybill NandiWaybillRow
  | RowShiftType NandiShiftTypeRow
  | RowScheduleType NandiScheduleTypeRow
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

decodeNandiRow :: NandiTable -> Value -> Either String NandiRow
decodeNandiRow RouteInternal v = RowRoute <$> parseEither parseJSON v
decodeNandiRow RoutePointInternal v = RowRoutePoint <$> parseEither parseJSON v
decodeNandiRow BusScheduleInternal v = RowBusSchedule <$> parseEither parseJSON v
decodeNandiRow BusScheduleTripInternal v = RowBusScheduleTrip <$> parseEither parseJSON v
decodeNandiRow BusScheduleTripDetailInternal v = RowBusScheduleTripDetail <$> parseEither parseJSON v
decodeNandiRow BusScheduleTripFlexiInternal v = RowBusScheduleTripFlexi <$> parseEither parseJSON v
decodeNandiRow ServiceTypeInternal v = RowServiceType <$> parseEither parseJSON v
decodeNandiRow StopInternal v = RowStop <$> parseEither parseJSON v
decodeNandiRow DesignationsInternal v = RowDesignation <$> parseEither parseJSON v
decodeNandiRow EmployeesInternal v = RowEmployee <$> parseEither parseJSON v
decodeNandiRow EntitiesInternal v = RowEntity <$> parseEither parseJSON v
decodeNandiRow VehiclesInternal v = RowVehicle <$> parseEither parseJSON v
decodeNandiRow WaybillDeviceInternal v = RowWaybillDevice <$> parseEither parseJSON v
decodeNandiRow FleetEtmMappingInternal v = RowFleetEtmMapping <$> parseEither parseJSON v
decodeNandiRow FleetObuMappingInternal v = RowFleetObuMapping <$> parseEither parseJSON v
decodeNandiRow WaybillsInternal v = RowWaybill <$> parseEither parseJSON v
decodeNandiRow BusShiftTypeInternal v = RowShiftType <$> parseEither parseJSON v
decodeNandiRow BusScheduleTypeInternal v = RowScheduleType <$> parseEither parseJSON v

data FilterOperator = Eq | NotEq | Gt | Lt | Like
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToSchema)

filterOperatorToText :: FilterOperator -> Text
filterOperatorToText Eq = "eq"
filterOperatorToText NotEq = "noteq"
filterOperatorToText Gt = "gt"
filterOperatorToText Lt = "lt"
filterOperatorToText Like = "like"

instance ToJSON FilterOperator where
  toJSON = toJSON . filterOperatorToText

instance FromJSON FilterOperator where
  parseJSON = withText "FilterOperator" $ \case
    "eq" -> pure Eq
    "noteq" -> pure NotEq
    "gt" -> pure Gt
    "lt" -> pure Lt
    "like" -> pure Like
    v -> fail $ "Unknown FilterOperator: " <> T.unpack v

data QueryFilter = QueryFilter
  { column :: Text,
    operator :: FilterOperator,
    value :: Text
  }
  deriving (Generic, ToSchema, Show)

instance FromJSON QueryFilter where
  parseJSON = withArray "QueryFilter" $ \vec -> do
    let elems = toList vec
    case elems of
      [col, op, val] ->
        QueryFilter
          <$> parseJSON col
          <*> parseJSON op
          <*> parseJSON val
      _ -> fail $ "QueryFilter array must have 3 elements, got: " ++ show (length elems)

instance ToJSON QueryFilter where
  toJSON (QueryFilter col op val) = toJSON [toJSON col, toJSON op, toJSON val]

data QueryBody = QueryBody
  { filters :: [QueryFilter],
    limit :: Maybe Int,
    offset :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets QueryBody where
  hideSecrets = identity
