{
module Front.Scanner where

import Front.Token (Token(..), TokenType(..), Span(..), tokenType)
import Debug.Trace
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

  -- | Emit closing braces.
  <closeBraces> {
    ()                  { (trace "closeBraces epsilon" closeBrace) }
  }

  <startBlock,startLine> {
    -- | Ignore consecutive newlines.
    @newline            { (trace "startLine newline" (\_ _ -> alexMonadScan)) }
    -- | First non-whitespace character of line.
    ()                  { (trace "startLine eps" firstLineToken) }
  }

  -- | Custom action for new lines.
  <0,startBlock> {
    @newline              { (trace "newline newline" nextLine) }
  }

  -- | Explicit matched delimiters, higher prio than 'firstBlockToken'.
  <0,startBlock> {
    \(                    { lDelimeter TLparen TRparen }
    \)                    { rDelimeter TRparen }
    \[                    { lDelimeter TLbracket TRbracket }
    \]                    { rDelimeter TRbracket }
    \{                    { lDelimeter TLbrace TRbrace }
    \}                    { rDelimeter TRbrace }
  }

  <startBlock> {
    -- | First non-whitespace character of block.
    ()                  { firstBlockToken }
  }

  <0> {
    -- | Keywords that start blocks
    if                    { layoutNL  TIf     TSemicolon }
    else                  { layout    TElse   TSemicolon }
    while                 { layoutNL  TWhile  TSemicolon }
    let                   { layout    TLet    TDBar }
    match                 { layoutNL  TMatch  TBar }
    loop                  { layout    TLoop   TSemicolon }
    do                    { layout    TDo     TSemicolon }
    par                   { layout    TPar    TDBar }
    wait                  { layout    TWait   TDBar }

    -- | Keywords that just do as they be
    after                 { keyword TAfter }
    \=                    { keyword TEq }
    \:                    { keyword TColon }
    \<\-                  { keyword TLarrow }
    \|\|                  { keyword TDBar }
    \-\>                  { keyword TRarrow }
    \|                    { keyword TBar }
    \;                    { keyword TSemicolon }
    \:                    { keyword TColon }
    \,                    { keyword TComma }
    \_                    { keyword TUnderscore }
    \@                    { keyword TAt }
    \&                    { keyword TAmpersand }

    -- | Other tokens
    @operator             { strTok TOp }
    \` @identifier \`     { strTok (TOp . dropEnds 1 1) }
    @identifier           { strTok TId }
    $digit+               { strTok (TInteger . read) }
  }

{
-- | User-facing syntax error.
syntaxErr :: String -> Alex a
syntaxErr s = alexError $ "Syntax error: " ++ s

-- | Internal compiler error for unreachable code.
internalErr :: String -> Alex a
internalErr s = alexError $ "Internal error: " ++ s

-- | The various contexts that the scanner maintains in its stack state.
data ScannerContext
  -- | In a freeform block with left margin, to end at given token.
  = Freeform Int TokenType
  -- | Ending explicit block with closing token and span.
  | EndingFreeform TokenType Span
  -- | Start a block at next token with given separator.
  | PendingBlock TokenType
  -- | Start a block at next line with given separator.
  | PendingBlockNL TokenType
  -- | In an implicit block with left margin, lines separated by given token.
  | InBlock Int TokenType
  deriving (Show)

-- | The state attached the 'Alex' monad; scanner maintains a stack of contexts.
data AlexUserState = AlexUserState { usContext :: [ScannerContext] }

-- | Initial Alex monad state.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { usContext = [InBlock 1 TDBar] }

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


-- | Arbitrary string token helper, which uses 'f' to produce 'TokenType'.
strTok :: (String -> TokenType) -> AlexAction Token
strTok f i@(_,_,_,s) len = do
  trace ("hit identifier '" ++ head (words s) ++ "'") $ return ()
  return $ Token (alexInputSpan i len, f $ take len s)

-- | Keyword is just a plain keyword, which just emits given 'TokenType'.
keyword :: TokenType -> AlexAction Token
keyword ttype i len = return $ Token (alexInputSpan i len, ttype)

-- | Keyword starts a new block immediately.
layout :: TokenType -> TokenType -> AlexAction Token
layout ttype sepToken i len = do
  -- Push 'PendingBlock' onto stack so we remember what 'sepToken' is
  alexPushContext $ PendingBlock sepToken
  -- Set 'startBlock' code so that scanner takes us to the next token, at which
  -- point 'firstBlockToken' is invoked.
  alexSetStartCode startBlock
  -- Emit given token.
  return $ Token (alexInputSpan i len, ttype)

-- | First token in a block (epsilon action).
firstBlockToken :: AlexAction Token
firstBlockToken (pn@(AlexPn _ _ col), _, _, _) _ = do
  trace "we're in firstBlockToken" $ return ()
  alexSetStartCode 0
  -- Check the top of the context stack to obtain saved separator token.
  ctxt <- alexPeekContext
  sepToken <- case ctxt of
    PendingBlock s -> do return s
    _ -> internalErr $ "unexpected context in firstBlockToken: " ++ show ctxt
  alexPopContext
  -- About to start a block; remember current indentation level and sepToken.
  alexPushContext $ InBlock col sepToken
  -- Emit TLbrace to start block.
  return $ Token (alexEmptySpan pn, TLbrace)

-- | Keyword starts a new block at next line.
layoutNL :: TokenType -> TokenType -> AlexAction Token
layoutNL ttype sepToken i len = do
  -- Push PendingBlockNL' onto stack so we remember 'sepToken', and to start
  -- new block at next line
  alexPushContext $ PendingBlockNL sepToken
  -- Emit token and continue scanning
  return $ Token (alexInputSpan i len, ttype)

-- | Left delimiting token, along with its (closing) right delimiter.
lDelimeter :: TokenType -> TokenType -> AlexAction Token
lDelimeter ttype closer i len = do
  ctxt <- alexPeekContext
  case (ctxt, ttype) of
    -- If about to start a block and we encounter explicit @{@, then that block
    -- has been started explicitly, so we forget about the 'PendingBlock' state.
    (PendingBlock _, TLbrace)   -> alexPopContext
    (PendingBlockNL _, TLbrace) -> alexPopContext
    _ -> return ()
  -- Push 'Freeform' to remember the column this explicit block was started,
  -- and what closing token to look for.
  let sp = alexInputSpan i len
  alexPushContext $ Freeform (tokCol sp) closer
  return $ Token (alexInputSpan i len, ttype)

-- | Right delimiter, e.g., ), }, ]
rDelimeter :: TokenType -> AlexAction Token
rDelimeter ttype i len = do
  alexPushContext $ EndingFreeform ttype (alexInputSpan i len)
  closeBrace i len

-- | "Subroutine" to insert closing braces (epsilon action).
closeBrace :: AlexAction Token
closeBrace (pn, _, _, _) _ = do
  alexSetStartCode 0
  -- When this subroutine starts, we expect the 'EndingFreeform' context we
  -- pushed to always be at the top of the stack; we use this to maintain state.
  closing <- alexPeekContext
  alexPopContext
  (closer, cspan) <- case closing of
    EndingFreeform cl sp -> return (cl, sp)
    _ -> internalErr $ "unexpected ctxt during closeBrace: " ++ show closing

  -- We check whatever context comes next.
  ctxt <- alexPeekContext
  alexPopContext
  case ctxt of
    -- If we are inside of another block, we close it by emitting an TRbrace,
    -- and loop back around to this subroutine. We also push the closing
    -- context back onto the stack for the next iter.
    InBlock _ _ -> do
      alexSetStartCode closeBraces
      alexPushContext closing
      return $ Token (alexEmptySpan pn, TRbrace)

    -- If we find the Freeform block we're supposed to close, then we're done.
    -- Reset the alex start code to the default (0), and emit closing delimiter.
    Freeform _ closer' | closer == closer' -> do
      return $ Token (cspan, closer)

    -- If somehow in Freeform for different closer, then we there must be
    -- a delimiter mismatch, e.g., @( ]@.
    Freeform _ _closer' -> syntaxErr "Delimiter mismatch error message TODO"

    -- If somehow pending block, then user wrote something like @do )@ or
    -- @if x )@, both of which are syntax errors.
    PendingBlock _   -> syntaxErr "error message TODO closeBrace pendingblockNL"
    PendingBlockNL _ -> syntaxErr "error message TODO clsoeBrace pendingblockNl"

    _ -> internalErr $ "unexpected ctxt during closeBrace: " ++ show ctxt

-- | Start of each line.
nextLine :: AlexAction Token
nextLine _ _ = do
  -- We're at the start of the line, but we don't yet know:
  -- (1) what the first token is, or
  -- (2) what its indentation is.
  -- We set the start code to 'startLine' and let the scanner take us there,
  -- at which point it will call 'firstLineToken'.
  alexSetStartCode startLine
  trace "we're in nextLine" $ return ()
  alexMonadScan

-- | First token in a line.
firstLineToken :: AlexAction Token
firstLineToken (_,_,_,"") _ = do
  -- EOF case; turn off 'startLine' and let scanner proceed to 'alexEOF'.
  trace "we hit firstLineToken EOF" $ return ()
  alexSetStartCode 0
  alexMonadScan
firstLineToken (pn@(AlexPn _ _ tCol), _, _, _) _ = do
  -- We have encountered the first token in a line.
  trace "we hit firstLineToken" $ return ()
  alexSetStartCode 0
  ctxt <- alexPeekContext
  case ctxt of
    InBlock col sepToken
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

    Freeform _ TRbrace -> do
      -- Newlines don't mean anything in freeform mode started by braces.
      alexMonadScan

    Freeform col _
      -- Adopt a technicality of Haskell: when freeform mode is started by
      -- non-braces, new lines must be greater than where freeform mode was
      -- started.
      | tCol > col -> do
        alexMonadScan
      | otherwise -> do
        syntaxErr "error message TODO firstLineToken"

    PendingBlockNL sepToken -> do
      -- We were about to start a block in a new line, and we encountered the
      -- first token of that new line. Transition from 'PendingNL' to 'InBlock',
      -- and emit the opening brace for the new block.
      alexPopContext
      alexPushContext $ InBlock tCol sepToken
      return $ Token (alexEmptySpan pn, TLbrace)

    PendingBlock sepToken -> do
      -- We were about to start a new block immediately, but encountered
      -- a newline, and now we're at the first token of the new line---this
      -- happens when we have a hanging block. We start the block here.
      alexPopContext
      alexPushContext $ InBlock tCol sepToken
      return $ Token (alexEmptySpan pn, TLbrace)

    _ -> internalErr $ "unexpected ctxt during firstLineToken: " ++ show ctxt

-- | Called when Alex reaches EOF.
alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{ alex_pos = pos, alex_ust = st } ->
  case usContext st of
    -- Last block---emit 'TEOF'
    [InBlock _ _] -> Right (s, Token (alexEmptySpan pos, TEOF))
    -- Close all unclosed implicit blocks
    InBlock _ _ : ctxts@_ -> Right (s', Token (alexEmptySpan pos, TRbrace))
      where s' = s { alex_ust = st { usContext = ctxts } }
    _ -> error $ "error message TODO (alexEOF)" ++ show (usContext st)

-- | Used to integrate with Happy parser.
lexerForHappy :: (Token -> Alex a) -> Alex a
lexerForHappy = (alexMonadScan >>=)

-- | Extract a token stream from an input string.
scanTokens :: String -> Either String [Token]
scanTokens s = runAlex s getStream
  where
    getStream = do
     tok <- alexMonadScan
     case tok of
       Token (_, TEOF) -> return []
       _ -> (:) tok <$> getStream

-- | Extract a stream of token types (without span) from an input string.
scanTokenTypes :: String -> Either String [TokenType]
scanTokenTypes = fmap (map tokenType) . scanTokens
}
