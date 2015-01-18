module Language.TypeScript.Parser  where

import Data.Maybe
import Data.Array (map)
import Data.Array.Unsafe (init, last)
import Data.Tuple
import Data.Either
import Data.Foldable (foldl, notElem)

import Language.TypeScript.Types
import Language.TypeScript.Lexer

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators

import Control.Apply ((<*), (*>))
import Control.Alt
import Control.Lazy (defer1)
import Control.Alternative (many)
import Control.Monad.Error (strMsg)

import Control.Monad.Trampoline

type TokenParser a = ParserT [PosToken] Trampoline a

eof :: TokenParser Unit
eof = ParserT $ \ts ->
  return $ case ts of
    [] ->       { consumed: false, input: ts, result: Right unit }
    (t : _)  -> { consumed: false, input: ts, result: Left (strMsg $ "Expected EOF, found token " ++ show t.token ++ " at line " ++ show t.line ++ ", column " ++ show t.column) }

match :: forall a. (Token -> Maybe a) -> String -> TokenParser a
match f exp = ParserT \ts -> do
  return $ case ts of
    t : ts' -> case f t.token of
                 Just a ->  { consumed: true,  input: ts', result: Right a }
                 Nothing -> { consumed: false, input: ts , result: Left (strMsg ("Expected " ++ exp ++ ", found token " ++ show t.token ++ " at line " ++ show t.line ++ ", column " ++ show t.column)) }
    _       ->              { consumed: false, input: ts , result: Left (strMsg ("Expected " ++ exp ++ ", found EOF")) }

comments :: TokenParser (Maybe Comment)
comments = ParserT \ts -> do
  return $ case ts of
    t : _ -> { consumed: false, input: ts, result: Right (Just (Comment t.comments)) }
    _     -> { consumed: false, input: ts, result: Right Nothing }
      
matchOne :: Token -> String -> TokenParser Token
matchOne t = match f
  where
  f t' | t == t' = Just t'
  f _ = Nothing

reserved :: String -> TokenParser Unit
reserved r = match f (show r)
  where
  f (IdentOrKeyword k) | k == r = Just unit
  f _ = Nothing
  
identifier :: TokenParser String
identifier = match f "identifier"
  where
  f (IdentOrKeyword i) | i `notElem` reservedNames = Just i
  f _ = Nothing  
  
  reservedNames = [ "break", "do", "instanceof", "typeof", "case", "else", "new"
                  , "var", "catch", "finally", "return", "void", "continue", "for"
                  , "switch", "while", "debugger", "function", "this", "with"
                  , "default", "if", "throw", "delete", "in", "try", "class", "enum"
                  , "extends", "super", "const", "export", "import", "implements"
                  , "let", "private", "public", "yield", "interface", "package"
                  , "protected", "static"
                  ]
  
integer :: TokenParser Number
integer = match f "integer"
  where
  f (Natural n) = Just n
  f _ = Nothing  
  
stringLiteral :: TokenParser String
stringLiteral = match f "string literal"
  where
  f (StringLiteral s) = Just s
  f _ = Nothing 

semi :: TokenParser Token
semi = matchOne Semi ";"

comma :: TokenParser Token
comma = matchOne Comma ","

semiSep :: forall a. TokenParser a -> TokenParser [a]
semiSep p = p `sepBy` semi

semiSep1 :: forall a. TokenParser a -> TokenParser [a]
semiSep1 p = p `sepBy1` semi

commaSep :: forall a. TokenParser a -> TokenParser [a]
commaSep p = p `sepBy` comma

commaSep1 :: forall a. TokenParser a -> TokenParser [a]
commaSep1 p = p `sepBy1` comma

dot :: TokenParser Token
dot = matchOne Dot "."

colon :: TokenParser Token
colon = matchOne Colon ":"

equals :: TokenParser Token
equals = matchOne Equals "="

questionMark :: TokenParser Token
questionMark = matchOne QuestionMark "?"

ellipsis :: TokenParser Token
ellipsis = matchOne Ellipsis "..."

arrow :: TokenParser Token
arrow = matchOne Arrow "=>"
  
parens :: forall a. TokenParser a -> TokenParser a
parens = between (matchOne LParen "(") (matchOne RParen ")")
  
braces :: forall a. TokenParser a -> TokenParser a
braces = between (matchOne LBrace "{") (matchOne RBrace "}")
  
squares :: forall a. TokenParser a -> TokenParser a
squares = between (matchOne LSquare "[") (matchOne RSquare "]")
  
angles :: forall a. TokenParser a -> TokenParser a
angles = between (matchOne LAngle "<") (matchOne RAngle ">")

declarationSourceFile :: TokenParser [DeclarationElement]
declarationSourceFile = many declarationElement <* eof

declarationElement :: TokenParser DeclarationElement 
declarationElement = do
  com <- comments
  choice $
    [ try $ InterfaceDeclaration <$> optionMaybe exported
                                 <*> interface com
    , ExportDeclaration <$> (try (reserved "export" *> equals) *> identifier)
    , do exp <- try (optionMaybe exported <* reserved "import")
         nm  <- identifier <* equals
         ExternalImportDeclaration exp nm <$> (reserved "require" *> parens stringLiteral <* semi)
           <|> ImportDeclaration exp nm <$> entityName
    , AmbientDeclaration com <$> optionMaybe exported
                             <*> (reserved "declare" *> ambientDeclaration)
    ]
  where
  entityName :: TokenParser EntityName
  entityName = toEntityName <$> sepBy1 identifier dot
    where
    toEntityName [t] = EntityName Nothing t
    toEntityName ts = EntityName (Just $ ModuleName $ init ts) (last ts)

ambientDeclaration :: TokenParser Ambient
ambientDeclaration = do
  com <- comments
  defer1 $ \_ -> choice
    [ AmbientVariableDeclaration com <$> (reserved "var" *> identifier)
                                     <*> (optionMaybe typeAnnotation <* semi)
    , AmbientFunctionDeclaration com <$> (reserved "function" *> identifier)
                                     <*> (parameterListAndReturnType <* semi)
    , AmbientClassDeclaration com <$> (reserved "class" *> identifier)
                                  <*> optionMaybe typeParameters 
                                  <*> optionMaybe extendsClause 
                                  <*> optionMaybe implementsClause 
                                  <*> braces (ambientClassBodyElement `sepEndBy` semi)
    , AmbientInterfaceDeclaration <$> interface com
    , AmbientEnumDeclaration com <$> (reserved "enum" *> identifier)
                                 <*> braces (enumMember `sepEndBy` comma)
    , do reserved "module"
         AmbientModuleDeclaration com <$> sepBy identifier dot <*> braces (many (defer1 \_ -> try ambientDeclaration))
           <|> AmbientExternalModuleDeclaration com <$> stringLiteral
                                                    <*> braces (many (defer1 \_ -> try ambientExternalModuleElement))
    ]
  where
  enumMember :: TokenParser (Tuple String (Maybe Number))
  enumMember = Tuple <$> propertyName 
                     <*> optionMaybe (equals *> integer)
                     
  ambientClassBodyElement :: TokenParser (Tuple (Maybe Comment) AmbientClassBodyElement)
  ambientClassBodyElement = Tuple <$> comments <*> choice
    [ AmbientConstructorDeclaration <$> (reserved "constructor" *> parens parameterList)
    , try $ AmbientMemberDeclaration <$> optionMaybe publicOrPrivate
                                     <*> optionMaybe static
                                     <*> propertyName 
                                     <*> choice [ Right <$> parameterListAndReturnType
                                                , Left <$> optionMaybe typeAnnotation
                                                ]
    , AmbientIndexSignature <$> indexSignature 
    ]
  
ambientExternalModuleElement :: TokenParser AmbientExternalModuleElement
ambientExternalModuleElement = choice
  [ AmbientModuleElement <$> try (defer1 \_ -> ambientDeclaration)
  , exportAssignment
  , externalImportDeclaration ]
  where
  exportAssignment :: TokenParser AmbientExternalModuleElement
  exportAssignment = ExportAssignment <$> (try (reserved "export" *> equals) *> identifier <* semi)
  
  externalImportDeclaration :: TokenParser AmbientExternalModuleElement
  externalImportDeclaration =
    AmbientModuleExternalImportDeclaration <$> try (optionMaybe exported <* reserved "import")
                                           <*> identifier
                                           <*> (equals *> reserved "require" *> stringLiteral)
                                         
interface :: Maybe Comment -> TokenParser Interface
interface com = Interface com <$> (reserved "interface" *> identifier)
                              <*> optionMaybe typeParameters 
                              <*> optionMaybe extendsClause 
                              <*> objectType
                      
extendsClause :: TokenParser [TypeRef]
extendsClause = reserved "extends" *> classOrInterfaceTypeList

implementsClause :: TokenParser [TypeRef]
implementsClause = reserved "implements" *> classOrInterfaceTypeList

classOrInterfaceTypeList :: TokenParser [TypeRef]
classOrInterfaceTypeList = commaSep typeRef

objectType :: TokenParser TypeBody
objectType = defer1 \_ -> braces typeBody

typeBody :: TokenParser TypeBody
typeBody = TypeBody <$> sepEndBy typeMember semi
  where
  typeMember :: TokenParser (Tuple (Maybe Comment) TypeMember)
  typeMember = Tuple <$> comments
                     <*> defer1 \_ -> choice 
                           [ constructSignature
                           , try methodSignature
                           , try propertySignature
                           , try callSignature
                           , try typeIndexSignature 
                           ]

  constructSignature :: TokenParser TypeMember
  constructSignature = ConstructSignature <$> (reserved "new" *> defer1 \_ -> optionMaybe typeParameters) 
                                          <*> defer1 (\_ -> parens parameterList)
                                          <*> defer1 (\_ -> optionMaybe typeAnnotation)
  

  methodSignature :: TokenParser TypeMember
  methodSignature = MethodSignature <$> propertyName 
                                    <*> optionMaybe ((questionMark *> return Optional)) 
                                    <*> defer1 (\_ -> parameterListAndReturnType)
  
  propertySignature :: TokenParser TypeMember
  propertySignature = PropertySignature <$> propertyName 
                                        <*> optionMaybe ((questionMark *> return Optional)) 
                                        <*> defer1 \_ -> optionMaybe typeAnnotation

  callSignature :: TokenParser TypeMember
  callSignature = CallSignature <$> defer1 \_ -> parameterListAndReturnType

  typeIndexSignature :: TokenParser TypeMember
  typeIndexSignature = TypeIndexSignature <$> defer1 \_ -> indexSignature

propertyName :: TokenParser String
propertyName = identifier <|> stringLiteral

typeAnnotation :: TokenParser Type
typeAnnotation = colon *> defer1 \_ -> _type

parameterListAndReturnType :: TokenParser ParameterListAndReturnType
parameterListAndReturnType = defer1 \_ -> 
  ParameterListAndReturnType <$> optionMaybe typeParameters 
                             <*> parens parameterList 
                             <*> optionMaybe typeAnnotation

parameterList :: TokenParser [Parameter]
parameterList = defer1 \_ -> commaSep parameter
  where
  parameter :: TokenParser Parameter
  parameter = defer1 \_ -> choice
    [ try $ RequiredOrOptionalParameter <$> optionMaybe publicOrPrivate 
                                        <*> identifier 
                                        <*> optionMaybe (questionMark *> return Optional)
                                        <*> optionMaybe typeAnnotation
    , RestParameter <$> (ellipsis *> identifier) 
                    <*> optionMaybe typeAnnotation
    ]

exported :: TokenParser Exported
exported = reserved "export" *> return Exported
  
static :: TokenParser Static
static = reserved "static" *> return Static

publicOrPrivate :: TokenParser PublicOrPrivate
publicOrPrivate = choice
  [ reserved "public" *> return Public
  , reserved "private" *> return Private ]
  
stringOrNumber :: TokenParser StringOrNumber
stringOrNumber = choice
  [ reserved "string" *> return String
  , reserved "number" *> return Number ]

indexSignature :: TokenParser IndexSignature
indexSignature = squares (IndexSignature <$> identifier <*> (colon *> stringOrNumber)) <*> defer1 \_ -> typeAnnotation

typeParameters :: TokenParser [TypeParameter]
typeParameters = defer1 \_ -> angles $ commaSep1 typeParameter
  where
  typeParameter :: TokenParser TypeParameter
  typeParameter = TypeParameter <$> identifier 
                                <*> optionMaybe (reserved "extends" *> defer1 \_ -> _type)

_type :: TokenParser Type
_type = defer1 \_ -> choice [ arrayType, functionType, constructorType ]
  where
  arrayType = fold atomicType (squares (return unit)) (flip $ const ArrayType)
  atomicType = defer1 \_ -> choice
    [ Predefined <$> predefinedType
    , TypeReference <$> typeRef
    , ObjectType <$> objectType
    ]
  functionType = defer1 \_ -> FunctionType <$> optionMaybe typeParameters <*> parens parameterList <*> returnType
  constructorType = defer1 \_ -> ConstructorType <$> (reserved "new" *> optionMaybe typeParameters) <*> parens parameterList <*> returnType
  returnType = arrow *> defer1 \_ -> _type
  
  fold :: forall s m a b. (Monad m) => ParserT s m a -> ParserT s m b -> (a -> b -> a) -> ParserT s m a
  fold first more combine = do
    a <- first
    bs <- many (try more)
    return $ foldl combine a bs
  
  predefinedType :: TokenParser PredefinedType
  predefinedType = choice
    [ reserved "any" *> return AnyType
    , reserved "number" *> return NumberType
    , (reserved "boolean" <|> reserved "bool") *> return BooleanType
    , reserved "string" *> return StringType
    , reserved "void" *> return VoidType
    ]

typeRef :: TokenParser TypeRef
typeRef = defer1 \_ -> TypeRef <$> typeName <*> optionMaybe typeArguments
  where
  typeName :: TokenParser TypeName
  typeName = toTypeName <$> sepBy1 identifier dot
    where
    toTypeName [t] = TypeName Nothing t
    toTypeName ts = TypeName (Just $ ModuleName $ init ts) (last ts)

  typeArguments :: TokenParser [Type]
  typeArguments = angles $ defer1 \_ -> commaSep1 _type
