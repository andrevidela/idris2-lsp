module Server.Run

import Stellar.System.Console
import Stellar.API

import Core.Context
import Core.Core
import Core.Directory
import Core.InitPrimitives
import Core.Metadata
import Core.UnifyState
import Compiler.Common
import Data.List1
import Data.String
import Idris.CommandLine
import Idris.Env
import Idris.REPL.Opts
import Idris.REPL.Common
import Idris.Package.Types
import Idris.SetOptions
import Idris.Syntax
import Idris.Version
import IdrisPaths

import Server.Configuration

[COREIO] HasIO Core using Monad.CORE where
  liftIO = coreLift

runServer : Ref LSPConf LSPConfiguration
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref MD Metadata
         => Ref ROpts REPLOpts
         => Core ()
runServer
  = let xx = replEffects @{CORE} {io = Core}
                 ""
                 (putStrHandler @{COREIO})
                 (getLineHandler @{COREIO})
                 (putStdErrLnHandler @{COREIO})
                 (putStrLnHandler @{COREIO})
                 ?lmao
    in ignore $ runCostate xx T
