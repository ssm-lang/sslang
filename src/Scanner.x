{

-- https://stackoverflow.com/questions/20315739/how-to-use-an-alex-monadic-lexer-with-happy

module Scanner where

import Data.Char ( isDigit )
import Duration
import Data.Word ( Word64 )
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
    
    if    { layoutNL TIf TThen TSemicolon}
    else  { layout   TElse TSemicolon }
    while { layoutNL TWhile TDo TSemicolon}
    let   { layout   TLet TAnd}
    case  { layoutNL TCase TOf TBar }
    loop  { layout   TLoop TSemicolon }
    \=    { layout   TEq TSemicolon }
    \:    { keyword TColon }
    do    { doBlock }
    par   { layout  TPar TDBar }
    later { keyword TLater }
    wait  { keyword TWait }

    \<\-  { layout  TLarrow TSemicolon }
    \|\|  { keyword TDBar }
    \;    { keyword TSemicolon }
    \:    { keyword TColon }
    \-\>  { keyword TRarrow }
    \|    { keyword TBar }
    \,    { keyword TComma }
    \_    { keyword TUnderscore }
    \@    { keyword TAt }

    \(    { lDelimeter TLparen }
    \)    { rDelimeter TRparen }
    \[    { lDelimeter TLbracket }
    \]    { rDelimeter TRbracket }
    \{    { lDelimeter TLbrace }
    \}    { rDelimeter TRbrace }

    $digit+ $blank* ns { duration             1 }
    $digit+ $blank* us { duration          1000 }
    $digit+ $blank* ms { duration       1000000 }
    $digit+ $blank* s  { duration    1000000000 }
    $digit+ $blank* m  { duration   60000000000 }
    $digit+ $blank* h  { duration 3600000000000 }
  
    $digit+ { \ (pos,_,_,s) len ->
                  return $ Token pos $ TInteger $ read $ take len s }

    @operator { \ (pos,_,_,s) len -> return $ Token pos $ TOp $ take len s }

    \` @identifier \` { \ (pos,_,_,_:s) len ->
                          return $ Token pos $ TOp $ take (len - 2) s }

    @identifier { \ (pos,_,_,s) len -> return $ Token pos $ TId $ take len s }
  }
  
{

-- Get the position of the current token from an AlexInput
inputPos :: AlexInput -> AlexPosn
inputPos (pos, _, _, _) = pos

-- Get the column of the current token
inputCol :: AlexInput -> Int
inputCol ((AlexPn _ _ col), _, _, _) = col

-- Plain keyword helper: lexeme is discarded
keyword :: TokenType -> AlexInput -> Int -> Alex Token
keyword ttype alexInput _ = return $ Token (inputPos alexInput) ttype

-- Layout keyword
layout :: TokenType -> TokenType -> AlexInput -> Int -> Alex Token
layout ttype sepToken alexInput _ = do alexPushContext $ StartBlock sepToken
                                       return $ Token (inputPos alexInput) ttype

-- Layout-next-line keyword: set to StartBlockNL state
layoutNL :: TokenType -> TokenType -> TokenType -> AlexInput -> Int
         -> Alex Token
layoutNL ttype startToken sepToken alexInput _ = do
   alexPushContext $ StartBlockNL startToken sepToken
   return $ Token (inputPos alexInput) ttype

-- Left delimiter, e.g., (, {, [
lDelimeter :: TokenType -> AlexInput -> Int -> Alex Token
lDelimeter ttype alexInput _ = do
  case ttype of
    TLbrace -> do
      ctxt <- alexPeekContext
      case ctxt of
        StartBlock _     -> alexPopContext -- About to start a block? Found it
        StartBlockNL _ _ -> alexPopContext -- About to start a block? Found it
        _                -> return ()
    _ -> return ()
  alexPushContext Freeform
  return $ Token (inputPos alexInput) ttype

-- Right delimiter, e.g., ), }, ]
rDelimeter :: TokenType -> AlexInput -> Int -> Alex Token
rDelimeter ttype alexInput _ = do alexPopContext
                                  alexPeekContext >>= alexSwitchContext
                                  return $ Token (inputPos alexInput) ttype

-- Immediately start a block
doBlock :: AlexInput -> Int -> Alex Token
doBlock _ _ = do alexPushContext $ StartBlock TSemicolon
                 alexMonadScan

-- Duration literals
duration :: Word64 -> AlexInput -> Int -> Alex Token
duration mult (pos,_,_,s) _ = return $ Token pos $ TDuration dur
  where dur = Duration $ mult * (read $ takeWhile isDigit s)


data ScannerContext =
    InBlock Int TokenType -- In a block with the given left margin and separator
  | StartBlock TokenType  -- Start a block at the next token
                          -- with the given separator
  | StartBlockNL TokenType TokenType  -- Start a block on the next line with the
                                      -- given start token and separator
  | Freeform     -- Outside a block; ignore indentation

data AlexUserState = AlexUserState { usContext :: [ScannerContext]
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { usContext = [InBlock 1 TSemicolon]
                                  }

{-
alexGetContext :: Alex [ScannerContext]
alexGetContext = usContext <$> alexGetUserState

alexSetContext :: [ScannerContext] -> Alex ()
alexSetContext ctxt = do st <- alexGetUserState
                         alexSetUserState $ st { usContext = ctxt }
-}

-- Enter a new context: push onto the stack and switch the start code
-- as appropriate
alexPushContext :: ScannerContext -> Alex ()
alexPushContext ctxt = do
  st <- alexGetUserState
  alexSetUserState $ st { usContext = ctxt : usContext st }
  alexSwitchContext ctxt

-- Set the start code appropriately for the given context
alexSwitchContext :: ScannerContext -> Alex ()
alexSwitchContext (StartBlock _) = alexSetStartCode startBlock
alexSwitchContext (InBlock _ _) = alexSetStartCode inBlock
alexSwitchContext (StartBlockNL _ _) = alexSetStartCode inBlock
alexSwitchContext Freeform = alexSetStartCode freeform

alexPeekContext :: Alex ScannerContext
alexPeekContext = do st <- alexGetUserState
                     case usContext st of
                       c:_ -> return c
                       []  -> alexError "internal error: peek at empty state"

alexPopContext :: Alex ()
alexPopContext = do
  st <- alexGetUserState
  case usContext st of
   _ : cs -> alexSetUserState $ st { usContext = cs }
   _      -> alexError "internal error: popped at empty state"

nextLine, firstBlockToken, firstLineToken :: AlexInput -> Int -> Alex Token

-- At the start of the line: check the current context, switching from
-- StartBlockNL to StartBlock if necessary, and continue scanning
nextLine alexInput _ = do
  ctxt <- alexPeekContext
  case ctxt of
    InBlock _ _ -> do alexSetStartCode startLine
                      alexMonadScan
    StartBlock _ -> alexMonadScan
    StartBlockNL startToken sepToken -> do
            alexPopContext
            alexPushContext $ StartBlock sepToken
            return $ Token (inputPos alexInput) startToken
    Freeform -> alexError "internal error: nextLine in Freeform?"

-- At the first token in a block, remove the current state,
-- enter a new block context based on this token, and return a TBegin token
firstBlockToken alexInput _ = do
   ctxt <- alexPeekContext
   let sepToken = case ctxt of StartBlock s -> s
                               _ -> TSemicolon -- should not happen
   alexPopContext -- should be in StartBlock
   alexPushContext $ InBlock (inputCol alexInput) sepToken
   return $ Token (inputPos alexInput) TLbrace

-- At the first token in a line in a block, check the offside rule
firstLineToken (_,_,_,"") _ = do alexSetStartCode inBlock -- EOF case
                                 alexMonadScan
firstLineToken alexInput _ = do
  ctxt <- alexPeekContext
  let tCol = inputCol alexInput
  case ctxt of
    InBlock col sepToken
      | tCol > col  -> do alexSetStartCode inBlock -- Continued line
                          alexMonadScan
      | tCol == col -> do alexSetStartCode inBlock -- Next line starts
                          return $ Token (inputPos alexInput) sepToken
      | otherwise   -> do alexPopContext -- but stay in startLine code
                          return $ Token (inputPos alexInput) TRbrace
    _ -> alexError "StartBlock or StartBlockNL at first line token?"

data Token = Token AlexPosn TokenType
  deriving (Eq, Show)

data TokenType =
    TEOF
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TPar
  | TLoop
  | TLet
  | TAnd
  | TCase
  | TOf
  | TLater
  | TWait
  | TEq
  | TLarrow
  | TRarrow
  | TDBar
  | TColon
  | TSemicolon
  | TBar
  | TComma
  | TUnderscore
  | TAt
  | TLparen
  | TRparen
  | TLbrace
  | TRbrace
  | TLbracket
  | TRbracket
  | TInteger Integer
  | TString String
  | TId String
  | TOp String
  | TDuration Duration
  deriving (Eq, Show)

lexerForHappy :: (Token -> Alex a) -> Alex a
lexerForHappy = (alexMonadScan >>=)

-- End any pending blocks
alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{ alex_pos = pos, alex_ust = st } ->
          case dropWhile isntBlock (usContext st) of
            InBlock _ _ : ctxts@(_:_) -> Right (s', Token pos TRbrace)
                where s' = s { alex_ust = st { usContext = ctxts } }
            _ -> Right (s, Token pos TEOF)
            where isntBlock (InBlock _ _) = False
                  isntBlock _ = True                  

}
