||| Helper functions for LSP server responses and notifications.
|||
||| (C) The Idris Community, 2021
module Server.Response

import Core.Context
import Core.Core
import Core.Env
import Core.FC
import Data.OneOf
import Data.String
import Idris.Pretty
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import JSON.RPC
import Language.LSP.Message
import Language.LSP.Message.Method
import Language.LSP.Utils
import Parser.Support
import Server.Configuration
import Server.Diagnostics
import Server.Log
import Server.Utils
import System.File

%hide Language.LSP.Message.Message.ResponseError

||| Header for messages on-the-wire.
||| By specification only Content-Length is mandatory.
header : Int -> String
header l = "Content-Length: \{show l}\{headerLineEnd}\{headerLineEnd}"

||| Response message for method not found or not implemented yet.
export
methodNotFound : RPC.Error
methodNotFound = MkError RPC.MethodNotFound "Method not implemented yet" Nothing

||| Response message for error while parsing a new message received.
export
parseError : RPC.Error
parseError = MkError RPC.ParseError "Parse error" Nothing

||| Response message for internal server errors.
export
internalError : String -> RPC.Error
internalError msg = MkError RPC.InternalError msg Nothing

||| Response message for invalid requests.
export
invalidRequest : String -> RPC.Error
invalidRequest msg = MkError InvalidRequest msg Nothing

||| Response message for invalid parameters to an otherwise valid method.
export
invalidParams : String -> RPC.Error
invalidParams msg = MkError InvalidParams msg Nothing

||| Response message for messages received before initialization.
export
serverNotInitialized : RPC.Error
serverNotInitialized = MkError (toErrorCode ServerNotInitialized) "" Nothing

writeResponse : Context.Ref LSPConf LSPConfiguration
             => JSON
             -> Core ()
writeResponse msg = do
  let body = encode msg
  let header = header (cast $ length body)
  outputHandle <- gets LSPConf outputHandle
  coreLift_ $ fPutStr outputHandle (header ++ body)
  coreLift_ $ fflush outputHandle

toParams : MessageParams method -> Parameters

||| Sends a new notification from the server to the client.
export
sendNotificationMessage : Context.Ref LSPConf LSPConfiguration
                       => (method : Method Server Notification)
                       -> (params : MessageParams method)
                       -> Core ()
sendNotificationMessage method params = do
  let msg = MkNotification (show method) (toParams params)
  writeResponse (toJSON msg)
  logI Channel "Sent notification message for method \{encode method}"
  logD Channel "Notification sent: \{encode msg}"

||| Sends a request from the server to the client ignoring the result
||| TODO when client sends response we fail to parse it
export
sendRequestMessage_ : Context.Ref LSPConf LSPConfiguration
                   => (method : Method Server Request)
                   -> (params : MessageParams method)
                   -> Core ()
sendRequestMessage_ method params = do
  requestId <- gets LSPConf nextRequestId
  let msg = toJSON $ MkRequest (show method) (toParams params) (IntegerId $ cast {to = Integer} requestId)
  update LSPConf ({ nextRequestId := requestId + 1 })
  writeResponse (toJSON msg)
  logI Channel "Sent request message for method \{encode $ toJSON method}"
  logD Channel "Request sent: \{encode msg}"

||| Sends a response message to a request received from the client.
export
sendResponseMessage : Context.Ref LSPConf LSPConfiguration
                   => (method : Method Client Request)
                   -> ResponseSuccess
                   -> Core ()
sendResponseMessage method resp = do
  let msg = toJSON resp
  writeResponse msg
  logI Channel "Sent response message for method \{encode method}"
  logD Channel "Response sent: \{encode msg}"

||| Sends an error response to an unknown method.
export
sendUnknownResponseMessage : Context.Ref LSPConf LSPConfiguration => RPC.Error -> Core ()
sendUnknownResponseMessage err = do
  -- The method type Initialize is irrelevant since the message is unknown, can use any method, the message would be the same.
  writeResponse (toJSON {a = ResponseError} (MkResponseError err NoId))
  logI Channel "Sent response to unknown method"

||| Sends a LSP `Diagnostic` message for a given, potentially versioned, source
||| that produces some errors.
|||
||| @caps The client capabilities related to diagnostics
||| @uri The source URI.
||| @version Optional source version.
||| @errs Errors produced while trying to check the source.
export
sendDiagnostics : Context.Ref LSPConf LSPConfiguration
               => Context.Ref Syn SyntaxInfo
               => Context.Ref Ctxt Defs
               => Context.Ref ROpts REPLOpts
               => (caps : Maybe PublishDiagnosticsClientCapabilities)
               -> (uri : DocumentURI)
               -> (version : Maybe Int)
               -> (warnings : List Warning)
               -> (errs : List Core.Error)
               -> Core ()
sendDiagnostics caps uri version warnings errs = do
  warningDiagnostics <- traverse (warningToDiagnostic caps uri) warnings
  errorDiagnostics <- traverse (errorToDiagnostic caps uri) errs
  let diagnostics = warningDiagnostics ++ errorDiagnostics
  let params = MkPublishDiagnosticsParams uri version diagnostics
  logI Diagnostic "Sending diagnostic message for \{show uri}"
  sendNotificationMessage TextDocumentPublishDiagnostics params
