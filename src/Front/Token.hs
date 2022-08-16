{-# LANGUAGE NamedFieldPuns #-}
-- | Sslang source code tokens.
module Front.Token where

import           Common.Identifiers
import           Common.Pretty

-- | Tokens extracted from source text.
newtype Token = Token (Span, TokenType)
  deriving (Show, Eq)

-- | Extract the 'TokenType' from a 'Token'.
tokenType :: Token -> TokenType
tokenType (Token (_, t)) = t

-- | The location of a token in the source text.
data Span = Span
  { tokPos  :: Int
  , tokLen  :: Int
  , tokLine :: Int
  , tokCol  :: Int
  }
  deriving (Eq, Show)

-- | The types of tokens that can appear in a sslang source file.
data TokenType
  = TEOF
  | TType
  | TIf
  | TElse
  | TWhile
  | TDo
  | TPar
  | TLoop
  | TLet
  | TMatch
  | TAfter
  | TWait
  | TFun
  | TExtern
  | TEq
  | TLarrow
  | TRarrow
  | TDRarrow
  | TDBar
  | TColon
  | TSemicolon
  | TBar
  | TComma
  | TUnderscore
  | TAt
  | TAmpersand
  | TLparen
  | TRparen
  | TLbrace
  | TRbrace
  | TLbracket
  | TRbracket
  | TInteger Integer
  | TString String
  | TId Identifier
  | TOp Identifier
  | TCSym Identifier
  | TCQuote String
  | TCBlock String
  deriving (Eq, Show)

{- | 'Pretty' instance for 'Token', good for dumping tokens for inspection.

Prints 'TokenType' via both its 'Pretty' and 'Show' instances for clarity.
-}
instance Pretty Token where
  pretty (Token (sp, tok)) =
    fill 15 (pretty sp)
      <+> fill 31 (pretty tok)
      <+> pretty "("
      <>  viaShow tok
      <>  pretty ")"

-- | 'Pretty' instance for 'Span'. Reports both line:col and [addr+len].
instance Pretty Span where
  pretty Span { tokPos, tokLen, tokLine, tokCol } =
    fill 8 (pretty tokLine <> pretty ":" <> pretty tokCol) <> fill
      8
      (pretty "[" <> pretty tokPos <> pretty "+" <> pretty tokLen <> pretty "]")

-- | 'Pretty' instance for 'TokenType'. Recovers strings from keywords.
instance Pretty TokenType where
  pretty TEOF         = mempty
  pretty TType        = pretty "type"
  pretty TIf          = pretty "if"
  pretty TElse        = pretty "else"
  pretty TWhile       = pretty "while"
  pretty TDo          = pretty "do"
  pretty TPar         = pretty "par"
  pretty TLoop        = pretty "loop"
  pretty TLet         = pretty "let"
  pretty TMatch       = pretty "match"
  pretty TAfter       = pretty "after"
  pretty TWait        = pretty "wait"
  pretty TFun         = pretty "fun"
  pretty TExtern      = pretty "extern"
  pretty TEq          = pretty "="
  pretty TDRarrow     = pretty "=>"
  pretty TLarrow      = pretty "<-"
  pretty TRarrow      = pretty "->"
  pretty TDBar        = pretty "||"
  pretty TColon       = pretty ":"
  pretty TSemicolon   = pretty ";"
  pretty TBar         = pretty "|"
  pretty TComma       = pretty ","
  pretty TUnderscore  = pretty "_"
  pretty TAt          = pretty "@"
  pretty TAmpersand   = pretty "&"
  pretty TLparen      = pretty "("
  pretty TRparen      = pretty ")"
  pretty TLbrace      = pretty "{"
  pretty TRbrace      = pretty "}"
  pretty TLbracket    = pretty "["
  pretty TRbracket    = pretty "]"
  pretty (TInteger i) = pretty $ show i
  pretty (TString  s) = pretty $ "\"" <> s <> "\""
  pretty (TId      i) = pretty i
  pretty (TOp      o) = pretty o
  pretty (TCSym    s) = pretty $ fromString "$" <> s
  pretty (TCQuote  s) = pretty $ fromString "$$" <> s <> fromString "$$"
  pretty (TCBlock  b) = pretty $ fromString "$$$" <> b <> fromString "$$$"

-- | Pretty print a list of tokens.
prettyTokens :: [Token] -> String
prettyTokens = unlines . map (show . pretty)
