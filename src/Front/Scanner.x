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
import Data.List (isPrefixOf)
import Data.Char (readLitChar)
}

%wrapper "monadUserState"

$digit   = 0-9
$blank   = [\ \t]
@newline = [\n] | [\r][\n] | [\r]
@identifier = [a-zA-Z_] [a-zA-Z0-9_']*

$symbolBase         = [\!\#\%\&\+\-\.\<\=\>\?\@\\\^\|\~]
$symbolLeading      = [$symbolBase\*]
$symbolAfterSlash   = [$symbolBase\_\:\"\']
$symbolAny          = [$symbolBase\_\:\"\'\*]

@operator = \/ | ( $symbolLeading | \/ $symbolAfterSlash ) ( $symbolAny | \/ $symbolAfterSlash )* \/?

@commentL = \/\*
@commentR = \*\/

@cSym = \$
@cQuoteL = \$\$
@cQuoteR = \$\$
@cBlockL = \$\$\$
@cBlockR = \$\$\$
@cInlineChunk = ( [^\$] | @newline )+ | \$

$asciiChar = [a-zA-Z0-9\!\#\$\%\&\*\+\.\/\<\=\>\?\@\^\|\-\~\:\[\]\(\)\{\}\ ]
$escapable = [\\ \' \" n r t]
@escapeChar = \\ $escapable
@litChar = @escapeChar | $asciiChar

tokens :-
  <commentBody> {
    $blank+             ;

    @commentL           { commentBegin }

    @commentR           { commentEnd }

    -- Ignore anything else inside a comment
    . | @newline        ;
  }

  <closeBraces> {
    -- Close implicit braces.
    ()                  { closeBrace }
  }

  <lineStart> {
    $blank+             ;
    "//".*              ;
    @commentL           { commentBegin }

    -- Ignore consecutive newlines.
    @newline            ;

    -- First non-whitespace character of line.
    ()                  { \i l -> updateMargin i >> lineFirstToken i l }
  }

  <blockStart> {
    $blank+             ;
    "//".*              ;
    @commentL           { commentBegin }

    -- If we're about to start a block, newlines don't matter.
    @newline            ;

    -- Explicitly start a block at lBrace.
    \{                  { lBrace }

    -- Implicitly start a block at first non-whitespace character of block.
    ()                  { blockFirstToken }
  }

    <blockLineStart> {
    $blank+             ;
    "//".*              ;
    @commentL           { commentBegin }

    -- If we're about to start a block, newlines don't matter.
    @newline            ;

    -- Explicitly start a block at lBrace.
    \{                  { \i l -> updateMargin i >> lBrace i l }

    -- Implicitly start a block at first non-whitespace character of block.
    ()                  { \i l -> updateMargin i >> blockFirstToken i l }
  }

  <0> {
    $blank+             ;
    "//".*              ;
    @commentL           { commentBegin }

    -- Use lineStart to move scanner to first non-whitespace token of line.
    @newline            { gotoStartLine }

    -- Explicit delimiters.
    \{                  { lBrace }
    \}                  { rDelimeter TRbrace }
    \(                  { lDelimeter TLparen TRparen }
    \)                  { rDelimeter TRparen }
    \[                  { lDelimeter TLbracket TRbracket }
    \]                  { rDelimeter TRbracket }

    -- Keywords that start blocks.
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
    type                { layoutNL  TType   TBar }

    -- Keywords that just do as they be.
    extern              { keyword TExtern }
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

    -- Reserved keywords.
    do                  { reserved TDo }
    break               { reserved TBreak }
    now                 { reserved TNow }
    \@\@                { reserved TAtAt }

    -- Other stringy tokens.
    @operator           { strTok (return . TOp . fromString) }
    \` @identifier \`   { strTok (return . TOp . fromString . dropEnds 1 1) }
    @identifier         { strTok (return . TId . fromString) }
    $digit+             { strTok (return . TInteger . read) }
    \' @litChar \'      { charTok }
    --\" @litChar+ \"     { strTok ((return . TString . fromString) <=< (unescape . dropEnds 1 1)) }
    \" @litChar* \"     { strTok (return . TString . fromString . dropEnds 1 1) }

    -- InlineC C code
    @cSym @identifier   { strTok (return . TCSym . fromString . dropEnds 1 0) }
    @cQuoteL            { cInlineBegin cQuoteBody }
    @cBlockL            { cInlineBegin cBlockBody }
  }

  <cQuoteBody> {
    @cQuoteR            { cInlineEnd TCQuote }
    @cQuoteR @cQuoteR   { cInlineChunk (const "$$") }
    @cInlineChunk       { cInlineChunk id }
  }

  <cBlockBody> {
    @cBlockR            { cInlineEnd TCBlock }
    @cBlockR @cBlockR   { cInlineChunk (const "$$$") }
    @cInlineChunk       { cInlineChunk id }
  }
{
-- | Internal compiler error for unreachable code.
internalErr :: String -> Alex a
internalErr s = alexError $ "_i:" ++ s

-- | User-facing syntax error.
syntaxErr :: String -> Alex a
syntaxErr s = alexError $ "_s:" ++ s

-- | User-facing lexer error.
lexErr :: AlexInput -> String -> Alex a
lexErr i s = alexError $ "_l:" ++ showAlexLineCol (alexPosition i) ++ ":" ++ s

-- | Convert Alex's String-encoded errors to Sslang 'Compiler.Error'.
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
  -- | In a block of inlinec C code, accumulating the inlined C code.
  | InlineC [String] AlexPosn
  deriving (Show)

-- | The 'Int' in 'ScannerContext' is the margin.
ctxMargin :: ScannerContext -> Int
ctxMargin (ExplicitBlock m _        ) = m
ctxMargin (EndingExplicitBlock m _ _) = m
ctxMargin (PendingBlock m _         ) = m
ctxMargin (PendingBlockNL m _       ) = m
ctxMargin (ImplicitBlock m _        ) = m
ctxMargin (InlineC _ _         ) = 0


-- | The state attached the 'Alex' monad; scanner maintains a stack of contexts.
data AlexUserState = AlexUserState
  { usContext :: [ScannerContext] -- ^ stack of contexts
  , commentLevel :: Word          -- ^ 0 means no block comment
  , lastCtxCode :: Int            -- ^ last seen scanning code before block
  , lastLineMargin :: Int         -- ^ previous line's margin
  }

-- | Initial Alex monad state.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { usContext = [ImplicitBlock 1 TDBar]
  , commentLevel = 0
  , lastCtxCode = 0
  , lastLineMargin = 0
  }

-- | Enter a new context by pushing it onto stack.
alexPushContext :: ScannerContext -> Alex ()
alexPushContext ctx = do
  st <- alexGetUserState
  alexSetUserState $ st { usContext = ctx : usContext st }

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


-- | Obtain current alexPosition from input.
alexPosition :: AlexInput -> AlexPosn
alexPosition (pn, _, _, _) = pn

-- | Obtain current alexColumn from position.
alexColumn :: AlexPosn -> Int
alexColumn (AlexPn _ _ c) = c

-- | Return a string reporting line:column of the position
showAlexLineCol :: AlexPosn -> String
showAlexLineCol (AlexPn _ l c) = show l ++ ":" ++ show c

-- | Obtain 'Span' from 'AlexPosn'.
alexPosnSpan :: AlexPosn -> Int -> Span
alexPosnSpan (AlexPn a l c) len =
  Span {tokPos = a, tokLen = len, tokLine = l, tokCol = c}

-- | Obtain 'Span' from 'AlexInput'.
alexInputSpan :: AlexInput -> Int -> Span
alexInputSpan (pos, _, _, _) = alexPosnSpan pos

-- | Construct empty 'Span' for current position, from 'AlexInput'.
alexEmptySpan :: AlexPosn -> Span
alexEmptySpan (AlexPn a l c) =
  Span {tokPos = a, tokLen = 0, tokLine = l, tokCol = c}

-- | Predicate to measure whether there is more input.
alexNoMoreInput :: AlexInput -> Bool
alexNoMoreInput (_, _, _, "") = True
alexNoMoreInput _             = False


-- | String processing helper that drops both ends of a string. Note: strict.
dropEnds :: Int -> Int -> String -> String
dropEnds b a = reverse . drop a . reverse . drop b


-- | Beginning of a block comment.
commentBegin :: AlexAction Token
commentBegin _ _ = do
  st <- alexGetUserState
  c <- alexGetStartCode
  alexSetUserState $ st
    { commentLevel = commentLevel st + 1
    , lastCtxCode = if commentLevel st == 0 then c
                    else lastCtxCode st
    }
  alexSetStartCode commentBody
  alexMonadScan


-- | End of a block comment.
commentEnd :: AlexAction Token
commentEnd i _ = do
  st <- alexGetUserState
  let lvl = commentLevel st

  if lvl == 0 then
    lexErr i "unexpected token outside of a block comment: */"
  else if lvl == 1 then
    alexSetStartCode $ lastCtxCode st
  else
    alexSetStartCode commentBody

  alexSetUserState $ st { commentLevel = commentLevel st - 1 }

  alexMonadScan


-- | Start of each line.
gotoStartLine :: AlexAction Token
gotoStartLine _ _ = do
  -- We've arrived at the start of a new line, but we don't yet know:
  -- (1) what the first token is, or
  -- (2) what its indentation is.
  -- We set the start code to 'lineStart' and let the scanner take us there,
  -- at which point it will call 'lineFirstToken'.
  alexSetStartCode lineStart
  alexMonadScan

-- | Extracts the margin of the previous line from the AlexUserState 
extractMargin :: AlexUserState -> Int
extractMargin (AlexUserState _ _ _ m) = m

-- | Update the previous line margin 
updateMargin :: AlexInput -> Alex ()
updateMargin i = do 
  let c = alexColumn $ alexPosition i
  st <- alexGetUserState
  alexSetUserState $ st { lastLineMargin = c } 
     

-- | First token in a line.
lineFirstToken :: AlexAction Token
lineFirstToken i len
  -- EOF case: turn off 'lineStart' and let scanner proceed to 'alexEOF'.
  | alexNoMoreInput i = alexSetStartCode 0 >> alexMonadScan

  -- Regular case
  | otherwise         = lineFirstToken' i len

-- | First token in a line that isn't the last line.
lineFirstToken' :: AlexAction Token
lineFirstToken' i _ = do
  st <- alexGetUserState 
  let currMarg = alexColumn $ alexPosition i
      emptySpan = alexEmptySpan $ alexPosition i
      (_, _, _, tokenStr) = i
      lineMargin = extractMargin st

  alexSetStartCode 0

  -- margin is the context margin
  ctx <- alexPeekContext
  case ctx of
    ExplicitBlock _ _ -> alexMonadScan
      -- Newlines don't mean anything in explicit blocks.

    ImplicitBlock ctxMarg sepToken
      | currMarg == ctxMarg -> do
          if needsSep tokenStr
             then return $ Token (emptySpan, sepToken)
             else alexMonadScan

      -- Examples of when the currMarg == ctxMarg:
      -- loop f
      --      g // curr > line margin && == ctx margin
      --      h // curr == line margin && == ctx margin
      --        x
      --      f // curr < line margin && == ctx margin
      
      -- loop f
      -- g // curr == line margin && == ctx margin
      --
      -- loop loop f
      --      g // curr > line margin && == ctx margin
      --
      -- f
      --   x
      -- g // curr < line margin && == ctx margin

      | currMarg < ctxMarg -> do
        alexSetStartCode lineStart
        alexPopContext
        return $ Token (emptySpan, TRbrace)
      -- If currMarg < ctxMarg, pop a context to compare currMarg with the previous ctxMarg

    
      | currMarg > ctxMarg && currMarg >= lineMargin -> do
        -- Continued line; continue to scan as normal
        alexMonadScan
      -- Example of when the currMarg > ctxMarg with 2 diff cases of lineMarg
      -- f
      --   x // > line margin && > ctx margin
      --   y // == line margin && > ctx margin

      | currMarg > ctxMarg && currMarg < lineMargin -> do
         lexErr i $ "user error, outdent better"

      -- this means curr < prevline margin; we are outdenting
      | otherwise -> do
         internalErr $ "Unreachable; boolean comparisons should have covered all cases"

  -- if pending block, check if currmarg is < prevline margin
    PendingBlockNL _ sepToken
      | currMarg < lineMargin ->
        lexErr i $ "cannot start block after a new line at lower indentation than before"

      -- If currmarg >= prevline
      | otherwise -> do
        -- We were about to start a block in a new line, and we encountered the
        -- first token of that new line. Transition from 'PendingNL' to
        -- 'ImplicitBlock', and emit the opening brace for the new block.
        alexPopContext
        alexPushContext $ ImplicitBlock currMarg sepToken
        return $ Token (emptySpan, TLbrace)

    _ -> internalErr $ "unexpected ctx during lineFirstToken: " ++ show ctx

-- | Whether a token needs a separator inserted if it starts a line.
--
-- The argument should be the rest of the text to be parsed (i.e., the fourth
-- element of the 'AlexInput' given to an 'AlexAction'). This function looks at
-- that string to the predict upcoming token, and decides based on that. This is
-- admittedly kind of a hack, but we need this to  prevent semicolons from being
-- inserted in front of @else@ when we write something like:
--
-- @@
-- if cond
--   expr1
-- else
--   expr2
-- @@
--
-- The correct scanner output should be @if cond { expr1 } else { expr2 }@, and
-- not @if cond { expr1 } ; else { expr2 }@.
needsSep :: String -> Bool
needsSep = not . isPrefixOf "else"

-- | Left brace token.
lBrace :: AlexAction Token
lBrace i len = do
  -- It's possible we arrived at this brace while in the blockStart code.
  -- While it doesn't matter how we got here, we should still reset the code to
  -- scan as normal after completing this action.
  alexSetStartCode 0

  ctx <- alexPeekContext
  case ctx of
    -- If about to start a block and we encounter explicit @{@, then that block
    -- has been started explicitly, so we forget about the 'PendingBlock' state.
    PendingBlock _ _   -> alexPopContext
    PendingBlockNL _ _ -> alexPopContext
    _                  -> return ()

  -- Proceed to start an explicit block.
  lDelimeter TLbrace TRbrace i len

-- | Left delimiting token, along with its (closing) right delimiter.
lDelimeter :: TokenType -> TokenType -> AlexAction Token
lDelimeter ttype closer i len = do
  -- Push 'ExplicitBlock' to remember the alexColumn where this block was
  -- started, and what closing token to look for.
  let sp = alexInputSpan i len
  alexPushContext $ ExplicitBlock (tokCol sp) closer
  return $ Token (sp, ttype)


-- | Right delimiter, e.g., ), }, ]
rDelimeter :: TokenType -> AlexAction Token
rDelimeter ttype i len = do
  marg <- ctxMargin <$> alexPeekContext
  alexPushContext $ EndingExplicitBlock marg ttype (alexInputSpan i len)
  closeBrace i len

-- | "Subroutine" to insert closing braces (epsilon action).
closeBrace :: AlexAction Token
closeBrace i _ = do
  -- Default behavior: return to normal scanning after executing this action.
  alexSetStartCode 0

  -- When this subroutine starts, we expect the 'EndingExplicitBlock' context we
  -- pushed to always be at the top of the stack; we use this to maintain state.
  closing <- alexPeekContext
  alexPopContext
  (closer, cspan) <- case closing of
    EndingExplicitBlock _ cl sp -> return (cl, sp)
    ctx' -> internalErr $
      "unexpected ctx during closeBrace (" ++ show closing ++ "): " ++ show ctx'

  -- We check whatever context comes next.
  ctx <- alexPeekContext
  alexPopContext
  case ctx of
    -- If we are inside of another block, we close it by emitting an TRbrace,
    -- and loop back around to this subroutine. We also push the closing
    -- context back onto the stack for the next iter.
    ImplicitBlock _ _ -> do
      alexSetStartCode closeBraces
      alexPushContext closing
      return $ Token (alexEmptySpan $ alexPosition i, TRbrace)

    -- If we find the ExplicitBlock block we're supposed to close, then we're done.
    -- Reset the alex start code to the default (0), and emit closing delimiter.
    ExplicitBlock _ closer' | closer == closer' -> do
      return $ Token (cspan, closer)

    -- If somehow in ExplicitBlock for different closer, then we there must be
    -- a delimiter mismatch, e.g., @( ]@.
    ExplicitBlock _ closer' -> lexErr i $ unlines
      [ "mismatched delimiter:"
      , "expected '" ++ show closer' ++ "'"
      , "got '" ++ show closer ++ "'"
      ]

    -- If pending block, then user wrote something like @loop )@ or -- @if x )@,
    -- both of which are syntax errors.
    PendingBlock _  _ -> lexErr i $
      "unexpected token: expected expression, got '" ++ show closer ++ "'"
    PendingBlockNL _ _ -> lexErr i $
      "unexpected token: expected expression, got '" ++ show closer ++ "'"

    ctx' -> internalErr $
      "unexpected ctx during closeBrace (" ++ show closing ++ "): " ++ show ctx'


-- | Keyword starts a new block immediately.
layout :: TokenType -> TokenType -> AlexAction Token
layout ttype sepToken i len = do
  margin <- ctxMargin <$> alexPeekContext

  -- Push 'PendingBlock' onto stack so we remember what 'sepToken' is
  alexPushContext $ PendingBlock margin sepToken

  -- Set 'blockStart' code so that scanner takes us to the next token,
  -- at which point 'blockFirstToken' is invoked.
  alexSetStartCode blockStart

  -- Emit layout token.
  return $ Token (alexInputSpan i len, ttype)


-- | First token in a block (epsilon action).
blockFirstToken :: AlexAction Token
blockFirstToken i _ = do
  st <- alexGetUserState
  let lineMargin = extractMargin st
  alexSetStartCode 0

  -- Check the top of the context stack to obtain saved separator token.
  ctx <- alexPeekContext
  sepToken <- case ctx of
    PendingBlock _ s -> alexPopContext >> return s
    _ -> internalErr $ "unexpected context in blockFirstToken: " ++ show ctx

  -- Assert that indentation of this token is greater than that of layout token.
  let currMarg = alexColumn $ alexPosition i
  when (currMarg <= lineMargin) $ do
    lexErr i $ "cannot start block at lower indentation than before"

  -- About to start a block; remember current indentation level and separator.
  alexPushContext $ ImplicitBlock currMarg sepToken

  -- Emit TLbrace to start block.
  return $ Token (alexEmptySpan $ alexPosition i, TLbrace)


-- | Keyword starts a new block at next line.
layoutNL :: TokenType -> TokenType -> AlexAction Token
layoutNL ttype sepToken i len = do
  -- Push PendingBlockNL' onto stack so we remember 'sepToken',
  -- and to start new block at next line.
  margin <- ctxMargin <$> alexPeekContext
  alexPushContext $ PendingBlockNL margin sepToken

  -- Emit token and continue scanning.
  return $ Token (alexInputSpan i len, ttype)


-- | Keyword is just a plain keyword, which just emits given 'TokenType'.
keyword :: TokenType -> AlexAction Token
keyword ttype i len = return $ Token (alexInputSpan i len, ttype)


-- | Keyword is reserved keyword and should not be used.
reserved :: TokenType -> AlexAction Token
reserved ttype i _ = lexErr i $ "keyword is reserved: '" ++ show ttype ++ "'"


-- | Arbitrary string token helper, which uses @f@ to produce 'TokenType'.
strTok :: (String -> Alex TokenType) -> AlexAction Token
strTok f i@(_,_,_,s) len = do
  t <- f $ take len s
  return $ Token (alexInputSpan i len, t)


-- | Parse a char literal into the corresponding literal.
charTok :: AlexAction Token
charTok i@(_,_,_,s) len = do
  let charStr = dropEnds 1 1 $ take len s
  str <- unescape charStr
  case str of
    [c] -> return $ Token (alexInputSpan i len, TInteger $ toInteger $ ord c)
    _ -> internalErr $ "Could not unescape char literal: '" ++ charStr ++ "'"


-- | Unescape the escaped characters in a String.
unescape :: String -> Alex String
unescape s = case readLitChar s of
  [(c, s')] -> do
    cs <- unescape s'
    return (c:cs)
  []  -> syntaxErr $ "Could not escape string: '" ++ s ++ "'"
  _:_ -> internalErr $ "Ambiguous escape for string: '" ++ s ++ "'"


-- | Start scanning token of inline C code.
cInlineBegin :: Int -> AlexAction Token
cInlineBegin code (pos,_,_,_) _ = do
  st <- alexGetUserState
  c <- alexGetStartCode
  alexSetUserState $ st { lastCtxCode = c }
  alexPushContext $ InlineC [] pos
  alexSetStartCode code
  alexMonadScan


-- | Consume chunk of inline C cocde, applying some transformer 'f'.
cInlineChunk :: (String -> String) -> AlexAction Token
cInlineChunk f (_,_,_,s) len = do
  ctx <- alexPeekContext
  case ctx of
    InlineC ss pos -> do
      alexPopContext
      alexPushContext $ InlineC (f (take len s) : ss) pos
      alexMonadScan
    _ -> internalErr $ "unexpected ctx during cBlockEnd: " ++ show ctx


-- | Complete scanning token of inline C code.
cInlineEnd :: (String -> TokenType) -> AlexAction Token
cInlineEnd f (AlexPn a' _ _,_,_,_) _ = do
  st <- alexGetUserState
  ctx <- alexPeekContext
  case ctx of
    InlineC ss pos@(AlexPn a _ _) -> do
      let len = a' - a
          s = foldr (flip (++)) "" ss
      alexSetStartCode $ lastCtxCode st
      alexPopContext
      return $ Token (alexPosnSpan pos len, f $ fromString s)
    _ -> internalErr $ "unexpected ctx during cBlockEnd: " ++ show ctx


-- | Called when Alex reaches EOF.
alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{ alex_pos = pos, alex_ust = st } ->
  case usContext st of
    -- Last block---emit 'TEOF'
    [ImplicitBlock _ _] -> Right (s, Token (alexEmptySpan pos, TEOF))
    -- Close all unclosed implicit blocks
    ImplicitBlock _ _ : ctxs@_ -> Right (s', Token (alexEmptySpan pos, TRbrace))
      where s' = s { alex_ust = st { usContext = ctxs } }
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
