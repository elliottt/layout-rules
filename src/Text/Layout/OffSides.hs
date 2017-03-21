
{-# LANGUAGE RecordWildCards #-}

module Text.Layout.OffSides (
    Layout(..),
    layout,
  ) where

import AlexTools


data Layout tok = Layout { beginsLayout :: tok -> Bool
                           -- ^ True when this token begins layout
  
                         , endsLayout :: tok -> Bool
                           -- ^ True when this token explicitly ends layout
  
                         , sep :: SourceRange -> Lexeme tok
                           -- ^ The separator token
  
                         , start :: SourceRange -> Lexeme tok
                           -- ^ Layout block starting token
  
                         , end :: SourceRange -> Lexeme tok
                           -- ^ Layout block ending token
                         }

layout :: Layout tok -> [Lexeme tok] -> [Lexeme tok]
layout Layout { .. } = go Nothing []
  where
  startCol SourceRange { sourceFrom = SourcePos { .. } } = sourceColumn

  currentLevel (loc : _) = startCol loc
  currentLevel []        = 0

  -- a new layout level has been started, emit a starting token, and push the
  -- current level on the stack.
  go Just{} stack (tok : toks) =
    let loc = lexemeRange tok
     in start loc : tok : go Nothing (loc:stack) toks

  go (Just loc) stack [] =
    start loc : go Nothing (loc : stack) []

  go Nothing stack ts@(tok : toks)

    -- when the next token would close the current level
    | startCol loc < currentLevel stack =
      end loc : go Nothing (tail stack) ts

    | beginsLayout (lexemeToken tok) =
      let sepToks | startCol loc == currentLevel stack = [sep loc]
                  | otherwise                          = []
       in sepToks ++ tok : go (Just loc) stack toks

    | endsLayout (lexemeToken tok) =
      end loc : tok : go Nothing (tail stack) toks

    | startCol loc == currentLevel stack =
      sep loc : tok : go Nothing stack toks

    | otherwise =
      tok : go Nothing stack toks

    where
    loc = lexemeRange tok

  go _ stack [] =
    [ end loc | loc <- stack ]
