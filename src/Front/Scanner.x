{
{-# OPTIONS_HADDOCK prune #-}
{- | Scanner for sslang.

In addition to tokenizing the input text stream, this scanner is responsible for
the following:

- Insert implicit open braces after any block-starting token, if necessary;
- Insert implicit separators for aligned lines within the same block; and
- Insert implicit close braces if an implicit block closed, either by
  a decrease in indentation, or by the closure of a surrounding explicit block.

As such, the scanner's state is enriched with a stack of contexts which it uses
to perform basic delimiter matching. It also relies on epsilon transitions to
emit implicit tokens.
-}
module Front.Scanner
  ( scanTokens
  , scanTokenTypes
  , lexerForHappy
  , Token(..)
  , TokenType(..)
  , Span(..)
  , Alex(..)
  , syntaxErr
  , internalErr
  , liftErr
  , runAlex
  ) where

import Front.Token (Token(..), TokenType(..), Span(..), tokenType)
import Control.Monad (when)
import Common.Compiler (Pass, Error(..), liftEither)
import Data.Bifunctor (first)
}

%wrapper "monadUserState"

$digit   = 0-9
$blank   = [\ \t]
@newline = [\n] | [\r][\n] | [\r]
@identifier = [a-zA-Z] [a-zA-Z0-9_']*
$symbol = [\!\#\$\%\&\*\+\-\.\/\<\=\>\?\@\\\^\|\~]
@operator = $symbol ( $symbol | [\_\:\"\'] )*

tokens :-

  -- | Always ignore horizontal whitespace
  $blank+               ;

  -- | Always ignore single-line comments
  "//".*                ;

  <closeBraces> {
    -- Close implicit braces.
    ()                  { closeBrace }
  }

  <startLine> {
    -- Ignore consecutive newlines.
    @newline            ;

    -- First non-whitespace character of line.
    ()                  { firstLineToken }
  }

  <startBlock> {
    -- If we're about to start a block, newlines don't matter.
    @newline            ;

    -- Explicitly start a block at lBrace.
    \{                  { lBrace }

    -- Implicitly start a block at first non-whitespace character of block.
    ()                  { firstBlockToken }
  }

  <0> {
    -- Use startLine to move scanner to first non-whitespace token of line.
    @newline            { gotoStartLine }

    -- Forcibly starts an implicit block.
    do                  { doBlock }

    -- | Explicit delimiters.
    \{                  { lBrace }
    \}                  { rDelimeter TRbrace }
    \(                  { lDelimeter TLparen TRparen }
    \)                  { rDelimeter TRparen }
    \[                  { lDelimeter TLbracket TRbracket }
    \]                  { rDelimeter TRbracket }

    -- | Keywords that start blocks.
    if                  { layoutNL  TIf     TSemicolon }
    else                { layout    TElse   TSemicolon }
    while               { layoutNL  TWhile  TSemicolon }
    let                 { layout    TLet    TDBar }
    match               { layoutNL  TMatch  TBar }
    loop                { layout    TLoop   TSemicolon }
    \=                  { layout    TEq     TSemicolon }
    \<\-                { layout    TLarrow TSemicolon }
    \=\>                { layout    TDRarrow TSemicolon}
    par                 { layout    TPar    TDBar }
    wait                { layout    TWait   TDBar }
    fun                 { layoutNL  TFun    TSemicolon }

    -- | Keywords that just do as they be.
    after               { keyword TAfter }
    \:                  { keyword TColon }
    \|\|                { keyword TDBar }
    \-\>                { keyword TRarrow }
    \|                  { keyword TBar }
    \;                  { keyword TSemicolon }
    \:                  { keyword TColon }
    \,                  { keyword TComma }
    \_                  { keyword TUnderscore }
    \@                  { keyword TAt }
    \&                  { keyword TAmpersand }

    -- | Other stringy tokens.
    @operator           { strTok (TOp . fromString) }
    \` @identifier \`   { strTok (TOp . fromString . dropEnds 1 1) }
    @identifier         { strTok (TId . fromString) }
    $digit+             { strTok (TInteger . read) }
  }

{
-- | Internal compiler error for unreachable code.
internalErr :: String -> Alex a
internalErr s = alexError $ "_i:" ++ s

-- | User-facing syntax error.
syntaxErr :: String -> Alex a
syntaxErr s = alexError $ "_s:" ++ s

lexErr :: String -> Alex a
lexErr s = alexError $ "_l:" ++ s

liftErr :: String -> Error
liftErr ('_':'i':':':e) = UnexpectedError $ fromString e
liftErr ('_':'s':':':e) = ParseError      $ fromString e
liftErr ('_':'l':':':e) = LexError        $ fromString e
liftErr e               = LexError        $ fromString e

-- | The various contexts that the scanner maintains in its stack state.
data ScannerContext
  -- | In a freeform block that ends at given token.
  = ExplicitBlock Int TokenType
  -- | Ending explicit block with closing token and span.
  | EndingExplicitBlock Int TokenType Span
  -- | Start a block at next token with given separator.
  | PendingBlock Int TokenType
  -- | Start a block at next line with given separator.
  | PendingBlockNL Int TokenType
  -- | In an implicit block, lines separated by given token.
  | ImplicitBlock Int TokenType
  deriving (Show)

-- | The 'Int' in 'ScannerContext' is the margin.
ctxtMargin :: ScannerContext -> Int
ctxtMargin (ExplicitBlock m _        ) = m
ctxtMargin (EndingExplicitBlock m _ _) = m
ctxtMargin (PendingBlock m _         ) = m
ctxtMargin (PendingBlockNL m _       ) = m
ctxtMargin (ImplicitBlock m _        ) = m

-- | The state attached the 'Alex' monad; scanner maintains a stack of contexts.
data AlexUserState = AlexUserState { usContext :: [ScannerContext] }

-- | Initial Alex monad state.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { usContext = [ImplicitBlock 1 TDBar] }

-- | Enter a new context by pushing it onto stack.
alexPushContext :: ScannerContext -> Alex ()
alexPushContext ctxt = do
  st <- alexGetUserState
  alexSetUserState $ st { usContext = ctxt : usContext st }

-- | Read head of context stack.
alexPeekContext :: Alex ScannerContext
alexPeekContext = do
  st <- alexGetUserState
  case usContext st of
    c:_ -> return c
    []  -> internalErr "peek at empty state"

-- | Remove head of context stack.
alexPopContext :: Alex ()
alexPopContext = do
  st <- alexGetUserState
  case usContext st of
    _:cs -> alexSetUserState $ st { usContext = cs }
    _    -> internalErr "popped at empty state"

-- | Obtain 'Span' from 'AlexInput'.
alexInputSpan :: AlexInput -> Int -> Span
alexInputSpan (AlexPn a l c, _, _, _) len =
  Span {tokPos = a, tokLen = len, tokLine = l, tokCol = c}

-- | Obtain 'Span' from 'AlexInput'.
alexEmptySpan :: AlexPosn -> Span
alexEmptySpan (AlexPn a l c) =
  Span {tokPos = a, tokLen = 0, tokLine = l, tokCol = c}

-- | String processing helper that drops both ends of a string. Note: strict.
dropEnds :: Int -> Int -> String -> String
dropEnds b a = reverse . drop a . reverse . drop b


-- | Start of each line.
gotoStartLine :: AlexAction Token
gotoStartLine _ _ = do
  -- We've arrived at the start of a new line, but we don't yet know:
  -- (1) what the first token is, or
  -- (2) what its indentation is.
  -- We set the start code to 'startLine' and let the scanner take us there,
  -- at which point it will call 'firstLineToken'.
  alexSetStartCode startLine
  alexMonadScan

-- | Do block.
doBlock :: AlexAction Token
doBlock i len = do
  ctxt <- alexPeekContext
  case ctxt of
    -- If we were planning on starting a new block in the next line, but we
    -- find a @do@ before then, then that block will be started immediately.
    -- We need to scan to the next token, where the block will be started.
    PendingBlockNL _ _ -> alexSetStartCode startBlock >> alexMonadScan

    -- Otherwise, @do@ just starts an explicit do-block.
    _ -> layout TDo TSemicolon i len

-- | Left brace token.
lBrace :: AlexAction Token
lBrace i@(AlexPn _ _ tCol, _, _, _) len = do
  alexSetStartCode 0
  ctxt <- alexPeekContext
  case ctxt of
    -- If about to start a block and we encounter explicit @{@, then that block
    -- has been started explicitly, so we forget about the 'PendingBlock' state.
    PendingBlock _ _   -> alexPopContext
    PendingBlockNL _ _ -> alexPopContext
    _ -> return ()

  let col = ctxtMargin ctxt
  when (tCol <= col) $ do
    syntaxErr $ "Cannot start block at lower indentation than before"

  -- Proceed to start an explicit block.
  lDelimeter TLbrace TRbrace i len

-- | First token in a block (epsilon action).
firstBlockToken :: AlexAction Token
firstBlockToken (pn@(AlexPn _ _ tCol), _, _, _) _ = do
  alexSetStartCode 0

  -- Check the top of the context stack to obtain saved separator token.
  ctxt <- alexPeekContext
  (col, sepToken) <- case ctxt of
    PendingBlock m s -> alexPopContext >> return (m, s)
    _ -> internalErr $ "unexpected context in firstBlockToken: " ++ show ctxt

  when (tCol <= col) $ do
    syntaxErr $ "Cannot start block at lower indentation than before"

  -- About to start a block; remember current indentation level and sepToken.
  alexPushContext $ ImplicitBlock tCol sepToken
  -- Emit TLbrace to start block.
  return $ Token (alexEmptySpan pn, TLbrace)

-- | First token in a line.
firstLineToken :: AlexAction Token
firstLineToken (_,_,_,"") _ = do
  -- EOF case; turn off 'startLine' and let scanner proceed to 'alexEOF'.
  alexSetStartCode 0
  alexMonadScan
firstLineToken (pn@(AlexPn _ _ tCol), _, _, _) _ = do
  -- We have encountered the first token in a line.
  alexSetStartCode 0
  ctxt <- alexPeekContext
  case ctxt of
    ImplicitBlock col sepToken
      | tCol > col  -> do
        -- Continued line; continue to scan as normal
        alexMonadScan
      | tCol == col -> do
        -- Next line started; insert 'sepToken'
        return $ Token (alexEmptySpan pn, sepToken)
      | otherwise   -> do
        -- Block ended; pop context and insert 'TRbrace', but return to
        -- 'startLine' to check for anything else to do; if more blocks are
        -- ending, those will need to be closed too
        alexSetStartCode startLine
        alexPopContext
        return $ Token (alexEmptySpan pn, TRbrace)

    ExplicitBlock _ TRbrace -> do
      -- Newlines don't mean anything in freeform mode started by braces.
      alexMonadScan

    ExplicitBlock col _
      -- Adopt a technicality of Haskell: when freeform mode is started by
      -- non-braces, new lines must be greater than where freeform mode was
      -- started.
      | tCol > col -> alexMonadScan
      | otherwise  -> syntaxErr "error message TODO firstLineToken"

    PendingBlockNL col sepToken 
      | tCol > col -> do
        -- We were about to start a block in a new line, and we encountered the
        -- first token of that new line. Transition from 'PendingNL' to
        -- 'ImplicitBlock', and emit the opening brace for the new block.
        alexPopContext
        alexPushContext $ ImplicitBlock tCol sepToken
        return $ Token (alexEmptySpan pn, TLbrace)
      | otherwise ->
          syntaxErr $ "Cannot start block at lower indentation than before"

    _ -> internalErr $ "unexpected ctxt during firstLineToken: " ++ show ctxt

-- | Left delimiting token, along with its (closing) right delimiter.
lDelimeter :: TokenType -> TokenType -> AlexAction Token
lDelimeter ttype closer i len = do
  -- Push 'ExplicitBlock' to remember the column where this block was started,
  -- and what closing token to look for.
  let sp = alexInputSpan i len
  alexPushContext $ ExplicitBlock (tokCol sp) closer
  return $ Token (alexInputSpan i len, ttype)

-- | Right delimiter, e.g., ), }, ]
rDelimeter :: TokenType -> AlexAction Token
rDelimeter ttype i len = do
  marg <- ctxtMargin <$> alexPeekContext
  alexPushContext $ EndingExplicitBlock marg ttype (alexInputSpan i len)
  closeBrace i len

-- | Keyword starts a new block immediately.
layout :: TokenType -> TokenType -> AlexAction Token
layout ttype sepToken i len = do
  marg <- ctxtMargin <$> alexPeekContext
  -- Push 'PendingBlock' onto stack so we remember what 'sepToken' is
  alexPushContext $ PendingBlock marg sepToken
  -- Set 'startBlock' code so that scanner takes us to the next token, at which
  -- point 'firstBlockToken' is invoked.
  alexSetStartCode startBlock
  -- Emit given token.
  return $ Token (alexInputSpan i len, ttype)

-- | Keyword starts a new block at next line.
layoutNL :: TokenType -> TokenType -> AlexAction Token
layoutNL ttype sepToken i len = do
  -- Push PendingBlockNL' onto stack so we remember 'sepToken', and to start
  -- new block at next line
  marg <- ctxtMargin <$> alexPeekContext
  alexPushContext $ PendingBlockNL marg sepToken
  -- Emit token and continue scanning
  return $ Token (alexInputSpan i len, ttype)

-- | Keyword is just a plain keyword, which just emits given 'TokenType'.
keyword :: TokenType -> AlexAction Token
keyword ttype i len = return $ Token (alexInputSpan i len, ttype)

-- | Arbitrary string token helper, which uses 'f' to produce 'TokenType'.
strTok :: (String -> TokenType) -> AlexAction Token
strTok f i@(_,_,_,s) len = do
  return $ Token (alexInputSpan i len, f $ take len s)

-- | "Subroutine" to insert closing braces (epsilon action).
closeBrace :: AlexAction Token
closeBrace (pn, _, _, _) _ = do
  alexSetStartCode 0
  -- When this subroutine starts, we expect the 'EndingExplicitBlock' context we
  -- pushed to always be at the top of the stack; we use this to maintain state.
  closing <- alexPeekContext
  alexPopContext
  (closer, cspan) <- case closing of
    EndingExplicitBlock _ cl sp -> return (cl, sp)
    _ -> internalErr $ "unexpected ctxt during closeBrace: " ++ show closing

  -- We check whatever context comes next.
  ctxt <- alexPeekContext
  alexPopContext
  case ctxt of
    -- If we are inside of another block, we close it by emitting an TRbrace,
    -- and loop back around to this subroutine. We also push the closing
    -- context back onto the stack for the next iter.
    ImplicitBlock _ _ -> do
      alexSetStartCode closeBraces
      alexPushContext closing
      return $ Token (alexEmptySpan pn, TRbrace)

    -- If we find the ExplicitBlock block we're supposed to close, then we're done.
    -- Reset the alex start code to the default (0), and emit closing delimiter.
    ExplicitBlock _ closer' | closer == closer' -> do
      return $ Token (cspan, closer)

    -- If somehow in ExplicitBlock for different closer, then we there must be
    -- a delimiter mismatch, e.g., @( ]@.
    ExplicitBlock _ _closer' -> syntaxErr "Delimiter mismatch error message TODO"

    -- If somehow pending block, then user wrote something like @do )@ or
    -- @if x )@, both of which are syntax errors.
    PendingBlock _  _ -> syntaxErr $ "closeBrace :" ++ show ctxt
    PendingBlockNL _ _ -> syntaxErr $ "closeBrace :" ++ show ctxt

    _ -> internalErr $ "unexpected ctxt during closeBrace: " ++ show ctxt


-- | Called when Alex reaches EOF.
alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{ alex_pos = pos, alex_ust = st } ->
  case usContext st of
    -- Last block---emit 'TEOF'
    [ImplicitBlock _ _] -> Right (s, Token (alexEmptySpan pos, TEOF))
    -- Close all unclosed implicit blocks
    ImplicitBlock _ _ : ctxts@_ -> Right (s', Token (alexEmptySpan pos, TRbrace))
      where s' = s { alex_ust = st { usContext = ctxts } }
    ctx -> Left $ "encountered EOF inside of non-implicit context: " ++ show ctx

-- | Used to integrate with Happy parser.
lexerForHappy :: (Token -> Alex a) -> Alex a
lexerForHappy = (alexMonadScan >>=)

-- | Alex monad which collects all tokens together into a list.
collectStream :: Alex [Token]
collectStream = do
  tok <- alexMonadScan
  case tok of
    Token (_, TEOF) -> return []
    _ -> (:) tok <$> collectStream

-- | Extract a token stream from an input string.
scanTokens :: String -> Pass [Token]
scanTokens = liftEither . first liftErr . flip runAlex collectStream

-- | Extract a stream of token types (without span) from an input string.
scanTokenTypes :: String -> Pass [TokenType]
scanTokenTypes = fmap (map tokenType) . scanTokens
}
