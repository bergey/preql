{
-- | Description: Parse antiquotes without validating SQL syntax
module Preql.QuasiQuoter.Raw.Lex where

import           Prelude hiding (LT, GT, lex)

}

%wrapper "monadUserState"

$digit = [0-9]
$haskell = $printable # [\}]
$sql = $printable # [\$]

tokens :-

    "$" $digit+ { lex' (NumberedParam . read . tail) }
    "${" $haskell+ "}" { lex' (HaskellParam . init . drop 2) }
    $sql+ { lex' Sql }


{

data LocToken = LocToken
     { loc :: AlexPosn
     , unLoc :: Token
     } deriving Show

data Token = Sql String
     | NumberedParam Word | HaskellParam String
     | EOF
     deriving (Show, Eq, Ord)

/* from https://github.com/dagit/happy-plus-alex/blob/master/src/Lexer.x */

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = filePath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- For nice parser error messages.
unLex :: Token -> String
unLex t = case t of
    Sql s -> s
    NumberedParam i -> '$' : show i
    HaskellParam s -> "${" ++ s ++ "}"
    EOF -> "<EOF>"

-- Unfortunately, we have to extract the matching bit of string ourselves...
lex' :: (String -> Token) -> AlexAction LocToken
lex' f = \(p,_,_,s) i -> return $ LocToken p (f (take i s))

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex LocToken
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' _len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

alexEOF :: Alex LocToken
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ LocToken p EOF

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

lexAll :: Alex [LocToken]
lexAll = do
    t <- alexMonadScan
    case unLoc t of
        EOF -> return [t]
        _ -> fmap (t :) lexAll

parseQuery' :: FilePath -> String -> Either String [LocToken]
parseQuery' fp s = runAlex' lexAll fp s

parseQuery :: FilePath -> String -> Either String [Token]
parseQuery fp s = map unLoc <$> parseQuery' fp s

}
