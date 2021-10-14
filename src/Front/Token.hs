{-# LANGUAGE NamedFieldPuns #-}
module Front.Token where

import           Prettyprinter                  ( (<+>)
                                                , Pretty(..)
                                                )

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
  | TAfter
  | TWait
  | TNew
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
  | TAmpersand
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
  deriving (Eq, Show)

instance Pretty TokenType where
  pretty TEOF         = mempty
  pretty TIf          = pretty "if"
  pretty TThen        = pretty "then"
  pretty TElse        = pretty "else"
  pretty TWhile       = pretty "while"
  pretty TDo          = pretty "do"
  pretty TPar         = pretty "par"
  pretty TLoop        = pretty "loop"
  pretty TLet         = pretty "let"
  pretty TAnd         = pretty "and"
  pretty TCase        = pretty "case"
  pretty TOf          = pretty "of"
  pretty TAfter       = pretty "after"
  pretty TWait        = pretty "wait"
  pretty TNew         = pretty "new"
  pretty TEq          = pretty "="
  pretty TLarrow      = pretty "->"
  pretty TRarrow      = pretty "<-"
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
  pretty (TString  s) = pretty $ "\"" ++ s ++ "\""
  pretty (TId      i) = pretty i
  pretty (TOp      o) = pretty o

instance Pretty Span where
  pretty Span { tokPos, tokLen, tokLine, tokCol } =
    pretty (show tokLine ++ ":" ++ show tokCol)
      <+> pretty ("[" ++ show tokPos ++ "+" ++ show tokLen ++ "]")

instance Pretty Token where
  pretty (Token (sp, tok)) =
    pretty sp <+> pretty tok <+> pretty ("(" ++ show tok ++ ")")
