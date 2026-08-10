module Lib.Finance.Domain.Types.Extra.FinanceTdsReimbursementRequest where

import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (Log)
import Kernel.Utils.Error (throwError)
import Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest (AssessmentYear (..))

-- | Validates the Indian assessment-year format "YYYY-YY" (e.g. "2024-25"),
-- where the trailing two digits must be the next year, zero-padded.
mkAssessmentYear :: (MonadThrow m, Log m) => Text -> m AssessmentYear
mkAssessmentYear raw = case T.splitOn "-" raw of
  [startText, endText]
    | T.length startText == 4,
      T.length endText == 2,
      Just start <- readMaybe (T.unpack startText) :: Maybe Int,
      Just end <- readMaybe (T.unpack endText) :: Maybe Int,
      end == (start + 1) `mod` 100 ->
      pure $ AssessmentYear raw
  _ -> throwError $ InvalidRequest $ "Invalid assessment year \"" <> raw <> "\", expected format YYYY-YY, e.g. 2024-25"
