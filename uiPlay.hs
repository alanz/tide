{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import qualified TextBuffer
import qualified TokenBuffer
-- import qualified HaskellLex
import qualified PlayLex
import qualified Lex
import Span
import qualified Play

import Prelude hiding (lex, span)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.IORef
import Control.Monad
import Control.Exception
import Control.Concurrent
import System.Timeout (timeout)
import Graphics.UI.Gtk

main :: IO ()
main = do
  void $ initGUI

  window <- windowNew
  windowSetDefaultSize window 600 480
  void $ on window objectDestroy
    mainQuit

  scrolledWindow <- scrolledWindowNew Nothing Nothing
  textView <- textViewNew

  fontDescriptionFromString ("monospace 12" :: Text)
    >>= widgetModifyFont textView . Just

  set scrolledWindow [containerChild := textView]
  set window [containerChild := scrolledWindow]

  -- lexRef <- newIORef (Lex.empty HaskellLex.initialState HaskellLex.next)
  lexRef <- newIORef (Lex.empty PlayLex.initialState PlayLex.next)

  buffer <- textViewGetBuffer textView

  do
    tagTable <- textBufferGetTagTable buffer

    let
      table :: [(TagName,Text)]
      table =
          [ ("tokena" ,"green")
          , ("tokenbl","violet")
          , ("tokenbu","orange")
          , ("tokenbd","pink")
          , ("tokenbD","red")
          , ("tokenc" ,"blue")
          , ("comment","gray")
          ]
    forM_ table $ \(tagName, fgColour) -> do
      reservedIdTag <- textTagNew (Just tagName)
      set reservedIdTag [textTagForeground := fgColour]
      textTagTableAdd tagTable reservedIdTag

  updateVar <- newEmptyMVar

  let dumpLex = do
        do
          lex <- readIORef lexRef
          let ltxt = Text.length (TextBuffer.toText (Lex.textBuffer lex))
          let ltok = TokenBuffer.chars (TokenBuffer.measure (Lex.tokenBuffer lex))
          print (ltxt, ltok, Lex.isDirty lex)

        when False $ do
          lex <- readIORef lexRef
          Text.putStrLn (TextBuffer.toText $ Lex.textBuffer lex)
          print (TokenBuffer.toList $ Lex.tokenBuffer lex)
          print (map (fmap Lex.token) $ TokenBuffer.toList $ Lex.tokenBuffer lex)

      edit span txt = do
        lex <- readIORef lexRef
        let (lex', span') = Lex.replace lex span txt
        writeIORef lexRef lex'

        clear span'

      step = do
        lex <- readIORef lexRef
        when (Lex.isDirty lex) $ do
          let (lex', span) = Lex.step lex
          writeIORef lexRef lex'
          mask_ $ highlight span
          step

      update = do
        void $ timeout (1 * 1000) step
        l <- readIORef lexRef
        when (Lex.isDirty l) $ do
          void $ tryPutMVar updateVar ()

      updateThread = forever $ do
        threadDelay (10 * 1000)
        takeMVar updateVar
        postGUIAsync update

      clear span = do
        iter1 <- textBufferGetIterAtOffset buffer (start span)
        iter2 <- textBufferGetIterAtOffset buffer (start span + count span)
        textBufferRemoveAllTags buffer iter1 iter2

      highlight span = do
        do
          lex <- readIORef lexRef
          print ("HL" :: String, span, Lex.isDirty lex)

        clear span
        lex <- readIORef lexRef
        let (startPos, tokens) = Lex.tokens lex span
        go startPos tokens
        where
        go _ [] = return ()
        go pos (item : rest) = do
          hl pos item
          let pos' = case item of
                Left l -> pos + l
                Right i -> pos + Lex.consumedLength i
          go pos' rest
        hl _ Left{} = return ()
        hl pos (Right i) = do
          let maybe_tag = case Lex.token i of
                PlayLex.TokenA  -> Just ("tokena" :: Text)
                PlayLex.TokenBL -> Just "tokenbl"
                PlayLex.TokenBU -> Just "tokenbu"
                PlayLex.TokenBd -> Just "tokenbd"
                PlayLex.TokenBD -> Just "tokenbD"
                PlayLex.TokenC  -> Just "tokenc"
                PlayLex.White (PlayLex.Comment _)      -> Just "comment"
                PlayLex.White (PlayLex.CommentStart _) -> Just "comment"
                PlayLex.White (PlayLex.CommentEnd _)   -> Just "comment"
                _ -> Nothing
          case maybe_tag of
            Nothing -> return ()
            Just tag -> do
              let pos' = pos + Lex.consumedLength i
              iter1 <- textBufferGetIterAtOffset buffer pos
              iter2 <- textBufferGetIterAtOffset buffer pos'
              textBufferApplyTagByName buffer tag iter1 iter2

  void $ on buffer bufferInsertText $ \iter txt -> do
    off <- textIterGetOffset iter
    edit (Span off 0) txt

  void $ after buffer bufferInsertText $ \_ txt -> do
    let _ = txt :: Text
    update
    dumpLex

  void $ on buffer deleteRange $ \iter1 iter2 -> do
    off1 <- textIterGetOffset iter1
    off2 <- textIterGetOffset iter2
    edit (Span off1 (off2 - off1)) Text.empty

  void $ after buffer deleteRange $ \_ _ -> do
    update
    dumpLex

  void $ forkIO updateThread

  widgetShowAll window
  mainGUI
