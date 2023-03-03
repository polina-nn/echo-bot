-- | TelegramException errors leading to a bot stop, requiring the involvement of a engineer
module FrontEnd.TelegramException where

import qualified Control.Exception.Safe as EX
import qualified Network.HTTP.Client as NC
import qualified Network.HTTP.Req as Req

data BotException
  = -- | ServiceAPIError  -- ERROR while communicating with Telegram services (Token is invalid)
    ServiceAPIError String
  | -- | NetworkError -- Network communication ERROR (Problem whith Internet)
    NetworkError EX.SomeException
  | -- FormatError -- Failed while decoding response from Telegram failed
    FormatError String

instance EX.Exception BotException

rethrowReqException :: EX.MonadThrow m => Req.HttpException -> m a
rethrowReqException (Req.VanillaHttpException (NC.HttpExceptionRequest _ (NC.StatusCodeException resp _))) =
  EX.throw (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (Req.VanillaHttpException e) =
  EX.throw (NetworkError $ EX.toException e)
rethrowReqException (Req.JsonHttpException s) = EX.throw (FormatError s)

instance Show BotException where
  show (ServiceAPIError _) =
    "ERROR while communicating with Telegram services. You must check the token in the 'config.conf' file. "
  show (NetworkError _) =
    "Network communication ERROR. You must check your Internet. "
  show (FormatError _) = "Format ERROR. Contact the developer! Please"
