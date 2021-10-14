{
module Front.Scanner where
import Front.Token (Token(..), TokenType(..), Span(..))
}

%wrapper "monadUserState"

$digit   = 0-9
$blank   = [\ \t]
@newline = [\n] | [\r][\n] | [\r]
@identifier = [a-zA-Z] [a-zA-Z0-9_']*
$symbol = [\!\#\$\%\&\*\+\-\.\/\<\=\>\?\@\\\^\|\~]
@operator = $symbol ( $symbol | [\_\:\"\'] )*

tokens :-

  $blank+ ; -- Whitespace

  "//".*  ; -- Single-line comments; always just ignore them

  <startBlock> {
    @newline    ;
    ()          { firstBlockToken } -- First token in a new block
  }

  <startLine> {
    @newline    ;
    ()          { firstLineToken } -- First token on the line
  }

  <freeform> {
    @newline    ;
  }

  <0,inBlock> {
    @newline { nextLine }
  }

  <0,freeform,inBlock> {
    if    { layoutNL TIf TThen TSemicolon }
    else  { layout   TElse TSemicolon }
    while { layoutNL TWhile TDo TSemicolon }
    let   { layout   TLet TAnd}
    case  { layoutNL TCase TOf TBar }
    loop  { layout   TLoop TSemicolon }
    \=    { layout   TEq TSemicolon }
    \:    { keyword TColon }
    do    { doBlock }
    par   { layout  TPar TDBar }
    after { keyword TAfter }
    wait  { layout  TWait TDBar }
    new   { layout  TNew TSemicolon }

    \<\-  { layout  TLarrow TSemicolon }
    \|\|  { keyword TDBar }
    \;    { keyword TSemicolon }
    \:    { keyword TColon }
    \-\>  { keyword TRarrow }
    \|    { keyword TBar }
    \,    { keyword TComma }
    \_    { keyword TUnderscore }
    \@    { keyword TAt }
    \&    { keyword TAmpersand }

    \(    { lDelimeter TLparen }
    \)    { rDelimeter TRparen }
    \[    { lDelimeter TLbracket }
    \]    { rDelimeter TRbracket }
    \{    { lDelimeter TLbrace }
    \}    { rDelimeter TRbrace }

--    $digit+ $blank* ns { duration             1 }
--    $digit+ $blank* us { duration          1000 }
--    $digit+ $blank* ms { duration       1000000 }
--    $digit+ $blank* s  { duration    1000000000 }
--    $digit+ $blank* m  { duration   60000000000 }
--    $digit+ $blank* h  { duration 3600000000000 }

    $digit+                 { strTok (TInteger . read) }
    @operator               { strTok TOp }
    \` @identifier \`       { strTok (TOp . dropEnds 1 1) }
    @identifier             { strTok TId }
  }

{
-- | The various contexts the scanner encounters.
data ScannerContext
  = Freeform                            -- ^ Outside a block; ignore indentation
  | InBlock Int TokenType               -- ^ In a block with given left margin and separator
  | StartBlock TokenType                -- ^ Start a block at next token with given separator
  | StartBlockNL TokenType TokenType    -- ^ Start a block on next line with given start token and separator

-- | Set the scanner's start code for the given context.
alexSwitchContext :: ScannerContext -> Alex ()
alexSwitchContext Freeform              = alexSetStartCode freeform
alexSwitchContext (InBlock _ _)         = alexSetStartCode inBlock
alexSwitchContext (StartBlock _)        = alexSetStartCode startBlock
alexSwitchContext (StartBlockNL _ _)    = alexSetStartCode inBlock

-- | The state attached the 'Alex' monad; scanner maintains a stack of contexts.
data AlexUserState = AlexUserState { usContext :: [ScannerContext] }

-- | Initial Alex monad state.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { usContext = [InBlock 1 TSemicolon] }

-- | Enter a new context: push onto stack and switch start code as appropriate
alexPushContext :: ScannerContext -> Alex ()
alexPushContext ctxt = do
  st <- alexGetUserState
  alexSetUserState $ st { usContext = ctxt : usContext st }
  alexSwitchContext ctxt

-- | Read head of context stack.
alexPeekContext :: Alex ScannerContext
alexPeekContext = do
  st <- alexGetUserState
  case usContext st of
    c:_ -> return c
    []  -> alexError "internal error: peek at empty state"

-- | Remove head of context stack (NOTE: this does not switch context).
alexPopContext :: Alex ()
alexPopContext = do
  st <- alexGetUserState
  case usContext st of
    _:cs -> alexSetUserState (st { usContext = cs })
    _    -> alexError "internal error: popped at empty state"

-- | String processing helper that drops both ends of a string. Note: strict.
dropEnds :: Int -> Int -> String -> String
dropEnds b a = reverse . drop a . reverse . drop b

-- | Obtain 'Span' from 'AlexInput'.
alexInputSpan :: AlexInput -> Int -> Span
alexInputSpan (AlexPn a l c, _, _, _) len =
    Span {tokPos = a, tokLen = len, tokLine = l, tokCol = c}

-- | Obtain 'Span' from 'AlexInput'.
alexEmptySpan :: AlexPosn -> Span
alexEmptySpan (AlexPn a l c) =
    Span {tokPos = a, tokLen = 0, tokLine = l, tokCol = c}

-- | Get the column of the current input.
inputCol :: AlexInput -> Int
inputCol ((AlexPn _ _ col), _, _, _) = col

-- | Arbitrary string token helper, which uses 'f' to produce 'TokenType'.
strTok :: (String -> TokenType) -> AlexInput -> Int -> Alex Token
strTok f i@(_,_,_,s) len = return $ Token (alexInputSpan i len, f $ take len s)

-- | Keyword is just a plain keyword; emit given token.
keyword :: TokenType -> AlexInput -> Int -> Alex Token
keyword ttype i len = return $ Token (alexInputSpan i len, ttype)

-- | Keyword starts a new block; emit given token, but also push 'StartBlock'.
layout :: TokenType -> TokenType -> AlexInput -> Int -> Alex Token
layout ttype sepToken i len = do
    alexPushContext $ StartBlock sepToken
    return $ Token (alexInputSpan i len, ttype)

-- | Keyword starts a new block at 'startToken'; emit token and push 'StartBlockNL'.
layoutNL :: TokenType -> TokenType -> TokenType -> AlexInput -> Int -> Alex Token
layoutNL ttype startToken sepToken i len = do
   alexPushContext $ StartBlockNL startToken sepToken
   return $ Token (alexInputSpan i len, ttype)

-- | Left delimiter, e.g., (, {, [
lDelimeter :: TokenType -> AlexInput -> Int -> Alex Token
lDelimeter ttype i len = do
  case ttype of
    TLbrace -> do
      ctxt <- alexPeekContext
      case ctxt of
        StartBlock _     -> alexPopContext -- About to start a block? Found it
        StartBlockNL _ _ -> alexPopContext -- About to start a block? Found it
        _                -> return ()
    _ -> return ()
  alexPushContext Freeform
  return $ Token (alexInputSpan i len, ttype)

-- | Right delimiter, e.g., ), }, ]
rDelimeter :: TokenType -> AlexInput -> Int -> Alex Token
rDelimeter ttype i len = do
    alexPopContext
    alexPeekContext >>= alexSwitchContext
    return $ Token (alexInputSpan i len, ttype)

-- | Immediately start a block
doBlock :: AlexInput -> Int -> Alex Token
doBlock _ _ = do alexPushContext $ StartBlock TSemicolon
                 alexMonadScan

-- | At the start of the line: check the current context, switching from
-- 'StartBlockNL' to 'StartBlock' if necessary, and continue scanning
nextLine :: AlexInput -> Int -> Alex Token
nextLine i len = do
  ctxt <- alexPeekContext
  case ctxt of
    InBlock _ _ -> do alexSetStartCode startLine
                      alexMonadScan
    StartBlock _ -> alexMonadScan
    StartBlockNL startToken sepToken -> do
            alexPopContext
            alexPushContext $ StartBlock sepToken
            return $ Token (alexInputSpan i len, startToken)
    Freeform -> alexError "internal error: nextLine in Freeform?"

-- | At the first token in a block, remove the current state,
-- enter a new block context based on this token, and return a 'TBegin' token
firstBlockToken :: AlexInput -> Int -> Alex Token
firstBlockToken i len = do
   ctxt <- alexPeekContext
   let sepToken = case ctxt of StartBlock s -> s
                               _ -> TSemicolon -- should not happen
   alexPopContext -- should be in StartBlock
   alexPushContext $ InBlock (inputCol i) sepToken
   return $ Token (alexInputSpan i len, TLbrace)

-- | At the first token in a line in a block, check the offside rule
firstLineToken :: AlexInput -> Int -> Alex Token
firstLineToken (_,_,_,"") _ = do alexSetStartCode inBlock -- EOF case
                                 alexMonadScan
firstLineToken i len = do
  ctxt <- alexPeekContext
  let tCol = inputCol i
  case ctxt of
    InBlock col sepToken
      | tCol > col  -> do alexSetStartCode inBlock -- Continued line
                          alexMonadScan
      | tCol == col -> do alexSetStartCode inBlock -- Next line starts
                          return $ Token (alexInputSpan i len, sepToken)
      | otherwise   -> do alexPopContext -- but stay in startLine code
                          return $ Token (alexInputSpan i len, TRbrace)
    _ -> alexError "StartBlock or StartBlockNL at first line token?"

-- | End any pending blocks.
alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{ alex_pos = pos, alex_ust = st } ->
          case dropWhile isntBlock (usContext st) of
            InBlock _ _ : ctxts@(_:_) -> Right (s', Token (alexEmptySpan pos, TRbrace))
                where s' = s { alex_ust = st { usContext = ctxts } }
            _ -> Right (s, Token (alexEmptySpan pos, TEOF))
            where isntBlock (InBlock _ _) = False
                  isntBlock _ = True

lexerForHappy :: (Token -> Alex a) -> Alex a
lexerForHappy = (alexMonadScan >>=)
}
