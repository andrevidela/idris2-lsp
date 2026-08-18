module Server.Run

import Stellar.System.Console
import Stellar.API

runServer : Ref LSPConf LSPConfiguration
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref MD Metadata
         => Ref ROpts REPLOpts
         => Core ()
-- runServer = handleMessage >> runServer
runServer = runCostate (runREPL "" ?aa) T >> pure ()
