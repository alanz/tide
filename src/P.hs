module P where

import Lexer
import Control.Monad

import SrcLoc -- From GHC
import StringBuffer -- From GHC
import FastString -- From GHC

st = PState {
        buffer     = stringToStringBuffer "abc",
        last_tk    = Nothing,
        last_loc   = noSpan,
        last_len   = 0,
        loc        = nulLoc,
        lex_state  = [0]
     }

nulLoc :: RealSrcLoc
nulLoc = mkRealSrcLoc (fsLit "foo") 0 0

noSpan :: RealSrcSpan
noSpan = mkRealSrcSpan nulLoc nulLoc

-- foo =

foo :: IO ()
foo = do
  let
    x :: ParseResult Token
    x = unP lexToken st
  return ()

lexAll :: ParseResult [Token]
lexAll = unP go st
  where
    go = do
      tok <- lexer return
      case tok of
        ITeof -> return []
        _ -> liftM (tok:) go

{-

hscParseThingWithLocation :: (Outputable thing, Data thing) => String -> Int
                          -> Lexer.P thing -> String -> Hsc thing
hscParseThingWithLocation source linenumber parser str
  = withTiming getDynFlags
               (text "Parser [source]")
               (const ()) $ {-# SCC "Parser" #-} do
    dflags <- getDynFlags

    let buf = stringToStringBuffer str
        loc = mkRealSrcLoc (fsLit source) linenumber 1

    case unP parser (mkPState dflags buf loc) of
        PFailed warnFn span err -> do
            logWarningsReportErrors (warnFn dflags)
            handleWarnings
            let msg = mkPlainErrMsg dflags span err
            throwErrors $ unitBag msg

        POk pst thing -> do
            logWarningsReportErrors (getMessages pst dflags)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr thing)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed_ast "Parser AST" $
                                   showAstData NoBlankSrcSpan thing
            return thing
-}
