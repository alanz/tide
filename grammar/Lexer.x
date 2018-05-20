
{
{-# LANGUAGE StandaloneDeriving #-}
module Lexer where

import Control.Monad    ( unless, liftM )
import GHC.Exts
import Data.Char
import Control.Monad    ( mplus,ap,liftM )
import Control.Monad.Fail
import qualified Control.Monad.Fail as MonadFail
import Control.Applicative ((<$))
import Data.Word

-- GHC stuff
import StringBuffer
import SrcLoc
import FastString


}


$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  "a"  { \s -> TokenA }
  "b"  { \s -> TokenBL }
  "B"  { \s -> TokenBU }
  "d"  { \s -> TokenDL }
  "D"  { \s -> TokenDU }
  "c"  { \s -> TokenC }



{

data ParseResult a
  = POk PState a
  | PFailed SrcSpan String
deriving instance (Show a) => Show (ParseResult a)

-- ---------------------------------------------------------------------

data PState = PState {
        buffer     :: StringBuffer,
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan, -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,  -- current loc (end of prev token + 1)
        lex_state  :: [Int]
     } deriving Show


data AlexInput = AI RealSrcLoc StringBuffer

-- ---------------------------------------------------------------------

newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = returnP
  (<*>) = ap

instance Monad P where
  (>>=) = thenP
  fail = MonadFail.fail

instance MonadFail P where
  fail = failP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed span err -> PFailed span err

failP :: String -> P a
failP msg =
  P $ \s ->
    PFailed (RealSrcSpan (last_loc s)) msg


failMsgP :: String -> P a
failMsgP msg =
  P $ \s ->
    PFailed  (RealSrcSpan (last_loc s)) msg

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> String -> P a
failLocMsgP loc1 loc2 str =
  P $ \s ->
    PFailed  (RealSrcSpan (mkRealSrcSpan loc1 loc2)) str

failSpanMsgP :: SrcSpan -> String -> P a
failSpanMsgP span msg =
  P $ \s ->
    PFailed span msg

getPState :: P PState
getPState = P $ \s -> POk s s

-- ---------------------------------------------------------------------

data Token
      = TokenA
      | TokenBL
      | TokenBU
      | TokenDL
      | TokenDU
      | TokenC

      | ITeof                       -- end of file token
 deriving Show

-- ---------------------------------------------------------------------

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s{ loc=l, buffer=b } ()


-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: (Token -> P a) -> P a
lexer cont = do
  tok <- lexToken
  --trace ("token: " ++ show tok) $ do

  cont tok

-- ---------------------------------------------------------------------

lexToken :: P Token
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  case alexScanUser undefined inp sc of
    AlexEOF -> return ITeof
    AlexError (AI loc2 buf) ->
        reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        return (t 'a')

-- ---------------------------------------------------------------------

lexString' :: String -> [Token]
lexString' str =
  case lexString str of
    POk _s ts -> ts
    PFailed _span err -> error err

lexString :: String -> ParseResult [Token]
lexString str = unP go st
  where
    buf = stringToStringBuffer str
    loc = mkRealSrcLoc (fsLit "<lexer input>") 1 1
    st = mkPState buf loc
    go = do
      tok <- lexer return
      case tok of
        ITeof -> return []
        _ -> liftM (tok:) go

-- | Creates a parse state from a 'ParserFlags' value
mkPState :: StringBuffer -> RealSrcLoc -> PState
mkPState buf loc =
  PState {
      buffer        = buf,
      last_tk       = Nothing,
      last_loc      = mkRealSrcSpan loc loc,
      last_len      = 0,
      loc           = loc,
      lex_state     = [0]
    }

  
{-
lexTokenStream :: StringBuffer -> RealSrcLoc -> ParseResult [Token]
lexTokenStream buf loc = unP go initState
    where dflags' = gopt_set (gopt_unset dflags Opt_Haddock) Opt_KeepRawTokenStream
          initState = (mkPState dflags' buf loc) { use_pos_prags = False }
          go = do
            ltok <- lexer False return
            case ltok of
              L _ ITeof -> return []
              _ -> liftM (ltok:) go
-}
-- ---------------------------------------------------------------------

reportLexError :: RealSrcLoc -> RealSrcLoc -> StringBuffer -> [Char] -> P a
reportLexError loc1 loc2 buf str
  | atEnd buf = failLocMsgP loc1 loc2 (str ++ " at end of input")
  | otherwise =
  let c = fst (nextChar buf)
  in if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
     then failLocMsgP loc2 loc2 (str ++ " (UTF-8 decoding error)")
     else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = adjustChar c

-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (c, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c


{-# INLINE adjustChar #-}
adjustChar :: Char -> Word8
adjustChar c = fromIntegral $ ord adj_c
  where non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        uniidchar       = '\x07'

        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
          | otherwise =
                -- NB: The logic behind these definitions is also reflected
                -- in basicTypes/Lexeme.hs
                -- Any changes here should likely be reflected there.

                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic

-- Getting the previous 'Char' isn't enough here - we need to convert it into
-- the same format that 'alexGetByte' would have produced.
--
-- See Note [Unicode in Alex] and #13986.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = chr (fromIntegral (adjustChar pc))
  where pc = prevChar buf '\n'

}
