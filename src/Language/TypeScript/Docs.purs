module Language.TypeScript.Docs where
   
import Data.Tuple   
import Data.Maybe   
import Data.Either   
import Data.Monoid (mempty)
import Data.Array (map, mapMaybe, null, groupBy, sortBy)
import Data.Foldable (foldMap, foldl, mconcat, intercalate)
import Data.Function (on)

import Control.Alt ((<|>))
   
import qualified Data.String as S   

import Language.TypeScript.Types
import Language.TypeScript.Comments
    
data Content 
  = Text String 
  | Element String [Attr] Html
  
type Html = [Content]

type Attr = Tuple String String

htmlToString :: Html -> String
htmlToString = foldMap contentToString
  where
  contentToString :: Content -> String  
  contentToString (Text s) = escape s
  contentToString (Element name attrs children) = 
    "<" ++ S.joinWith " " (name : map attrToString attrs) ++ ">" ++ 
    htmlToString children ++ 
    "</" ++ name ++ ">"

  attrToString :: Attr -> String
  attrToString (Tuple name value) = name ++ "=\"" ++ escape value ++ "\""
  
escape :: String -> String
escape = S.replace "&"  "&amp;"
     >>> S.replace "\"" "&quot;"
     >>> S.replace "'"  "&#39;"
     >>> S.replace "<"  "&lt;"
     >>> S.replace ">"  "&gt;"

text :: String -> Html
text s = [Text s]

element :: String -> Html -> Html
element name children = [Element name [] children]

-- A convenience operator for adding attributes a la blaze-html

infixl 4 !
    
(!) :: (Html -> Html) -> Attr -> Html -> Html
(!) f attr = add attr <<< f
  where
  add :: Attr -> Html -> Html
  add _    [] = []
  add attr (Element name attrs children : xs) = (Element name (attrs ++ [attr]) children : xs)
  add _    xs@(_ : _) = xs

-- Elements

html :: Html -> Html
html = element "html"

head :: Html -> Html
head = element "head"

link :: Html -> Html
link = element "link"

body :: Html -> Html
body = element "body"

span :: Html -> Html
span = element "span"

p :: Html -> Html
p = element "p"

div :: Html -> Html
div = element "div"

ul :: Html -> Html
ul = element "ul"

li :: Html -> Html
li = element "li"

code :: Html -> Html
code = element "code"

a :: Html -> Html
a = element "a"

strong :: Html -> Html
strong = element "strong"

header :: Number -> Html -> Html
header 1 = element "h1"
header 2 = element "h2"
header 3 = element "h3"
header 4 = element "h4"
header 5 = element "h5"
header _ = element "h6"

-- Attributes

_class :: String -> Attr
_class = Tuple "class"

rel :: String -> Attr
rel = Tuple "rel"

href :: String -> Attr
href = Tuple "href"

name :: String -> Attr
name = Tuple "name"

-- Simple elements

sp :: Html
sp = text " "

withClass :: String -> String -> Html
withClass className s = span ! _class className $ text s

keyword :: String -> Html
keyword = withClass "keyword"

ident :: String -> Html
ident = withClass "identifier"

syntax :: String -> Html
syntax = withClass "syntax"

literal :: String -> Html
literal = withClass "literal"

comma :: Html
comma = syntax ","

colon :: Html
colon = syntax ":"

surround :: Html -> Html -> Html -> Html
surround open close inside = open ++ inside ++ close

parens :: Html -> Html
parens = surround (syntax "(") (syntax ")")

angles :: Html -> Html
angles = surround (syntax "<") (syntax ">")

braces :: Html -> Html
braces = surround (syntax "{") (syntax "}")

squares :: Html -> Html
squares = surround (syntax "[") (syntax "]")

-- Comments

renderComment :: Comment -> Html
renderComment = renderParsedComment <<< parseComment
  where
  renderParsedComment :: ParsedComment -> Html      
  renderParsedComment pc = mconcat
    [ foldMap (\line -> p ! _class "comments" $ text line) pc.text
    , foldMap (\(g@((Tuple key _):_)) -> 
        p (strong (text key)) <>
        ul (foldMap (li <<< renderCommentKeyValuePair) g)) (groupBy ((==) `on` fst) pc.other)
    ]
  
  renderCommentKeyValuePair :: Attr -> Html
  renderCommentKeyValuePair (Tuple "see" value) = linkToBookmark (S.split "." value) $ text value
  renderCommentKeyValuePair (Tuple _     value) = text value
  
-- Bookmarks  
  
bookmarkName :: [String] -> String
bookmarkName = S.joinWith "-"  
  
bookmark :: [String] -> Html
bookmark names = a ! name (bookmarkName names) $ mempty  
  
linkToBookmark :: [String] -> Html -> Html
linkToBookmark names = a ! href ("#" ++ bookmarkName names)

comparing :: forall a b. (Ord b) => (a -> b) -> a -> a -> Ordering
comparing f = compare `on` f

foldl1 :: forall a. (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

-- Interfaces

topLevelInterfaces :: [DeclarationElement] -> [Tuple (Maybe Exported) Interface]
topLevelInterfaces =
  sortBy (comparing $ (\(Tuple _ (Interface _ name _ _ _)) -> name))
  <<< map collectComments
  <<< groupBy ((==) `on` (\(Tuple _ (Interface _ name _ _ _)) -> name))
  <<< mapMaybe toInterface
  where
  toInterface (InterfaceDeclaration e (Interface com name tps exts body)) = Just (Tuple e (Interface com name tps exts body))
  toInterface _ = Nothing
  
  collectComments :: [Tuple (Maybe Exported) Interface] -> Tuple (Maybe Exported) Interface
  collectComments = foldl1 (\(Tuple e1 (Interface c1 name tps exts (TypeBody b1)))
                             (Tuple e2 (Interface c2 _    _   _    (TypeBody b2))) ->
                     (Tuple (e1 <|> e2) (Interface (c1 <> c2) name tps exts (TypeBody (b1 <> b2)))))

renderInterface :: [String] -> Number -> (Tuple (Maybe Exported) Interface) -> Html
renderInterface moduleName level (Tuple e (Interface com name typeParameters extends body)) = mconcat
  [ bookmark $ moduleName ++ [name]
  , header level $ ident name
  , div ! _class "interface" $
      (p ! _class "class_decl mono" $
        foldMap renderExported e <>
        keyword "interface" <>
        sp <>
        ident name <>
        (foldMap $ angles <<< intercalate (comma <> sp) <<< map (renderTypeParameter moduleName)) typeParameters <>
        foldMap (\e -> sp <> keyword "extends" <> sp <> intercalate (comma <> sp) (map (renderTypeReference moduleName) e)) extends) <>
      foldMap renderComment com <>
      renderTypeBody moduleName (ul <<< foldMap li) body
  ]
    
ambientInterfaceDeclarations :: 
  [{ exp :: Maybe Exported
   , amb :: Ambient
   , comments :: Maybe Comment 
   }] -> [Tuple (Maybe Exported) Interface]
ambientInterfaceDeclarations = mapMaybe p
  where
  p { exp: e
    , amb: AmbientInterfaceDeclaration (Interface com a b c d)
    , comments: com1
    } = Just (Tuple e (Interface ((<>) <$> com <*> com1) a b c d))
  p _ = Nothing

renderAmbientInterfaceDeclaration :: [String] -> Number -> Tuple (Maybe Exported) Interface -> Html
renderAmbientInterfaceDeclaration moduleName level = renderInterface moduleName level    
    
-- Ambient declarations

ambientDeclarations :: [DeclarationElement] -> [{ exp :: Maybe Exported, amb :: Ambient, comments :: Maybe Comment }]
ambientDeclarations = mapMaybe toAmbient
  where
  toAmbient (AmbientDeclaration comments exp amb) = Just { exp: exp, amb: amb, comments: comments }
  toAmbient _ = Nothing
  
renderAmbientDeclarations :: [String] -> Number -> [{ exp :: Maybe Exported, amb :: Ambient, comments :: Maybe Comment }] -> Html
renderAmbientDeclarations moduleName level ds = 
  let vars       = ambientVariableDeclarations ds
      functions  = ambientFunctionDeclarations ds
      classes    = ambientClassDeclarations ds
      interfaces = ambientInterfaceDeclarations ds
      enums      = ambientEnumDeclarations ds
      modules    = ambientModuleDeclarations ds
  in mconcat [ section ul li "Variables"  vars       (renderAmbientVariableDeclaration moduleName)
             , section ul li "Functions"  functions  (renderAmbientFunctionDeclaration moduleName)
             , section id id "Classes"    classes    (renderAmbientClassDeclaration moduleName (level + 1))
             , section id id "Interfaces" interfaces (renderAmbientInterfaceDeclaration moduleName (level + 1))
             , section id id "Enums"      enums      (renderAmbientEnumDeclaration moduleName)
             , section id id "Modules"    modules    (\o -> renderAmbientModuleDeclaration (moduleName ++ [o.name]) (level + 1) o)
             ]
  where
  section :: forall a. (Html -> Html) -> (Html -> Html) -> String -> [a] -> (a -> Html) -> Html
  section outer inner name xs render =
    ifM (not (null xs)) $ 
      div ! _class "section" $ mconcat
        [ header level (text name)
        , outer $ foldMap (inner <<< render) xs
        ]
          
-- Variables

ambientVariableDeclarations :: 
  [{ exp :: Maybe Exported
   , comments :: Maybe Comment
   , amb :: Ambient 
   }] -> 
  [{ exp      :: Maybe Exported
   , comments :: Maybe Comment
   , name     :: String
   , ty       :: Maybe Type 
   }]
ambientVariableDeclarations = mapMaybe p
  where
  p { exp: e
    , amb: AmbientVariableDeclaration com name ty
    , comments = com1 
    } = Just { exp: e
             , name: name
             , ty: ty
             , comments: (<>) <$> com <*> com1
             }
  p _ = Nothing

renderAmbientVariableDeclaration :: 
  [String] -> 
  { exp      :: Maybe Exported
  , comments :: Maybe Comment
  , name     :: String
  , ty       :: Maybe Type 
  } -> 
  Html
renderAmbientVariableDeclaration moduleName o =
  span $ code $ mconcat
    [ mconcat [ foldMap renderExported o.exp
              , keyword "var"
              , sp
              , ident o.name
              , foldMap (renderTypeAnnotation moduleName) o.ty
              ]
    , foldMap renderComment o.comments
    ]
    
-- Functions

ambientFunctionDeclarations :: 
  [{ exp :: Maybe Exported
   , comments :: Maybe Comment
   , amb :: Ambient 
   }] ->
  [{ exp      :: Maybe Exported
   , comments :: Maybe Comment
   , name     :: String 
   , plrt     :: ParameterListAndReturnType
   }]
ambientFunctionDeclarations = mapMaybe p
  where
  p { exp: e
    , amb: AmbientFunctionDeclaration com name ps
    , comments = com1 
    } = Just { exp: e
             , name: name
             , plrt: ps
             , comments: (<>) <$> com <*> com1
             }
  p _ = Nothing

renderAmbientFunctionDeclaration :: 
  [String] -> 
  { exp      :: Maybe Exported
  , comments :: Maybe Comment
  , name     :: String 
  , plrt     :: ParameterListAndReturnType
  } -> 
  Html
renderAmbientFunctionDeclaration moduleName o = mconcat
  [ span $ code $ mconcat
      [ foldMap renderExported o.exp
      , keyword "function"
      , sp
      , ident o.name
      , renderParameterListAndReturnType moduleName o.plrt
      ]
  , foldMap renderComment o.comments
  ]
  
-- Classes

ambientClassDeclarations :: 
  [{ exp :: Maybe Exported
   , comments :: Maybe Comment
   , amb :: Ambient 
   }] ->
  [{ exp :: Maybe Exported
   , comments :: Maybe Comment
   , name     :: String 
   , tps      :: Maybe [TypeParameter]
   , exts     :: Maybe [TypeRef]
   , imps     :: Maybe [TypeRef]
   , els      :: [Tuple (Maybe Comment) AmbientClassBodyElement]
   }]
ambientClassDeclarations = mapMaybe p
  where
  p { exp: e
    , amb: AmbientClassDeclaration com name tps exts imps els
    , comments = com1 
    } = Just { exp: e
             , comments: com <> com1
             , name: name
             , tps: tps
             , exts: exts
             , imps: imps
             , els: els
             }
  p _ = Nothing

renderAmbientClassDeclaration :: 
  [String] -> 
  Number -> 
  { exp :: Maybe Exported
  , comments :: Maybe Comment
  , name     :: String 
  , tps      :: Maybe [TypeParameter]
  , exts     :: Maybe [TypeRef]
  , imps     :: Maybe [TypeRef]
  , els      :: [Tuple (Maybe Comment) AmbientClassBodyElement]
  } -> 
  Html
renderAmbientClassDeclaration moduleName level o = mconcat
  [ bookmark $ moduleName ++ [o.name]
  , header level $ mconcat
      [ foldMap renderExported o.exp
      , text o.name
      ]
  , div ! _class "class" $ mconcat
      [ foldMap renderComment o.comments
      , p ! _class "class_decl mono" $ mconcat
          [ keyword "class"
          , sp
          , ident o.name
          , foldMap (angles <<< intercalate (comma <> sp) <<< map (renderTypeParameter moduleName)) o.tps
          , foldMap (\e -> sp <> keyword "extends" <> sp <> intercalate (comma <> sp) (map (renderTypeReference moduleName) e)) o.exts
          , foldMap (\e -> sp <> keyword "implements" <> sp <> intercalate (comma <> sp) (map (renderTypeReference moduleName) e)) o.imps
          ]
      , ul $ foldMap (li <<< renderAmbientClassBodyElement moduleName) o.els  
      ]
  ]
 
renderAmbientClassBodyElement :: [String] -> Tuple (Maybe Comment) AmbientClassBodyElement -> Html
renderAmbientClassBodyElement moduleName (Tuple com el) = mconcat
  [ span $ code $ renderAmbientClassBodyElement' el
  , foldMap renderComment com
  ]
  where
  renderAmbientClassBodyElement' (AmbientConstructorDeclaration args) = mconcat
    [ keyword "constructor"
    , parens $ intercalate (comma <> sp) $ map (renderParameter moduleName) args
    ]
  renderAmbientClassBodyElement' (AmbientMemberDeclaration publicOrPrivate static name (Left ty)) = mconcat
    [ foldMap renderPublicOrPrivate publicOrPrivate
    , foldMap renderStatic static
    , keyword "var"
    , sp
    , ident name
    , foldMap (renderTypeAnnotation moduleName) ty
    ]
  renderAmbientClassBodyElement' (AmbientMemberDeclaration publicOrPrivate static name (Right prt)) = mconcat
    [ foldMap renderPublicOrPrivate publicOrPrivate
    , foldMap renderStatic static
    , keyword "function"
    , sp
    , ident name
    , renderParameterListAndReturnType moduleName prt
    ]
  renderAmbientClassBodyElement' (AmbientIndexSignature indexSignature) = renderIndexSignature moduleName indexSignature 
  
-- Enums

ambientEnumDeclarations :: [{ exp :: Maybe Exported
   , amb :: Ambient
   , comments :: Maybe Comment
   }] -> 
  [{ exp  :: Maybe Exported
   , name :: String
   , els :: [Tuple String (Maybe Number)]
   , comments :: Maybe Comment
   }]
ambientEnumDeclarations = mapMaybe p
  where
  p { exp: e
    , amb: AmbientEnumDeclaration com1 name els
    , comments: com2
    } = Just { exp:  e
             , name: name
             , els: els
             , comments: (<>) <$> com1 <*> com2
             }
  p _ = Nothing

renderAmbientEnumDeclaration :: 
  [String] -> 
  { exp  :: Maybe Exported
  , name :: String
  , els :: [Tuple String (Maybe Number)]
  , comments :: Maybe Comment
  } -> 
  Html
renderAmbientEnumDeclaration moduleName o =
  div ! _class "enum" $ mconcat
    [ bookmark $ moduleName ++ [o.name]
    , foldMap renderComment o.comments
    , foldMap renderExported o.exp
    , keyword "enum"
    , sp
    , ident o.name
    , ul $ flip foldMap o.els $ \(Tuple name value) -> li $ code $ mconcat
        [ ident name
        , flip foldMap value $ \val -> mconcat
            [ sp
            , syntax "="
            , sp
            , literal (show val)  
            ]
        ]
    ]
  
-- Modules

ambientModuleDeclarations :: 
  [{ exp :: Maybe Exported
   , amb :: Ambient
   , comments :: Maybe Comment
   }] -> 
  [{ exp  :: Maybe Exported
   , name :: String
   , ambs :: [Ambient]
   , comments :: Maybe Comment
   }]
ambientModuleDeclarations =
  sortBy (comparing getName)
  <<< map collectComments
  <<< groupBy ((==) `on` getName)
  <<< mapMaybe p
  where
  p :: { exp :: Maybe Exported
       , amb :: Ambient
       , comments :: Maybe Comment
       } -> 
       Maybe { exp  :: Maybe Exported
             , name :: String
             , ambs :: [Ambient]
             , comments :: Maybe Comment
             }
  p { exp: e
    , amb: AmbientModuleDeclaration com1 names ambs
    , comments: com2
    } = Just { exp:  e
             , name: intercalate "." names
             , ambs: ambs
             , comments: (<>) <$> com1 <*> com2
             }
  p { exp: e
    , amb: AmbientExternalModuleDeclaration com1 name ambs
    , comments: com2
    } = Just { exp:  e
             , name: name
             , ambs: mapMaybe q ambs
             , comments: (<>) <$> com1 <*> com2
             }
  p _ = Nothing
  
  q :: AmbientExternalModuleElement -> Maybe Ambient
  q (AmbientModuleElement a) = Just a
  q _ = Nothing
  
  collectComments :: 
             [{ exp :: Maybe Exported
              , name :: String
              , ambs :: [Ambient]
              , comments :: Maybe Comment
              }] -> { exp  :: Maybe Exported
                    , name :: String
                    , ambs :: [Ambient]
                    , comments :: Maybe Comment
                    }
  collectComments = foldl1 $ \o1 o2 -> 
                      { exp:      o1.exp <|> o2.exp
                      , name:     o1.name
                      , ambs:     o1.ambs ++ o2.ambs
                      , comments: (<>) <$> o1.comments <*> o2.comments
                      }
  
  getName :: forall r. { name :: String | r } -> String
  getName o = o.name

renderAmbientModuleDeclaration :: 
  [String] -> 
  Number -> 
  { exp  :: Maybe Exported
  , name :: String
  , ambs :: [Ambient]
  , comments :: Maybe Comment
  } -> 
  Html
renderAmbientModuleDeclaration moduleName level o =
  div ! _class "section" $ mconcat
    [ bookmark moduleName
    , header level $ text $ "Module " ++ (intercalate "." moduleName)
    , foldMap renderComment o.comments
    , renderAmbientDeclarations moduleName (level + 1) $ map (\a -> { exp: o.exp, amb: a, comments: Nothing :: Maybe Comment }) o.ambs
    ]
  
-- Flags
  
renderExported :: Exported -> Html
renderExported _ = keyword "export" <> sp

renderStatic :: Static -> Html
renderStatic _ = keyword "static" <> sp

renderOptional :: Optional -> Html
renderOptional _ = syntax "?"

renderPublicOrPrivate :: PublicOrPrivate -> Html
renderPublicOrPrivate Public = keyword "public" <> sp
renderPublicOrPrivate Private = keyword "private" <> sp

renderStringOrNumber :: StringOrNumber -> Html
renderStringOrNumber String = keyword "string"
renderStringOrNumber Number = keyword "number"

-- Types

renderTypeParameter :: [String] -> TypeParameter -> Html
renderTypeParameter moduleName (TypeParameter name ext) = 
  ident name <>
  foldMap (\ty -> sp <> keyword "extends" <> sp <> renderType moduleName ty) ext

renderTypeReference :: [String] -> TypeRef -> Html
renderTypeReference moduleName (TypeRef name args) =
  renderTypeName moduleName name <>
  (foldMap $ angles <<< intercalate (comma <> sp) <<< map (renderType moduleName)) args
  
renderType :: [String] -> Type -> Html
renderType _ (Predefined pre) = keyword $ predefinedTypeToString pre
  where
  predefinedTypeToString :: PredefinedType -> String
  predefinedTypeToString AnyType     = "any"
  predefinedTypeToString NumberType  = "number"
  predefinedTypeToString BooleanType = "boolean"
  predefinedTypeToString StringType  = "string"
  predefinedTypeToString VoidType    = "void"
renderType moduleName (TypeReference tr) = renderTypeReference moduleName tr
renderType moduleName (ObjectType body) = renderTypeBody moduleName (braces <<< intercalate (comma <> sp)) body
renderType moduleName (ArrayType ty) = renderType moduleName ty <> syntax "[]"
renderType moduleName (FunctionType gens args ret) =
  foldMap (angles <<< intercalate (comma <> sp) <<< map (renderTypeParameter moduleName)) gens <>
  (parens $ intercalate (comma <> sp) $ map (renderParameter moduleName) args) <>
  renderTypeAnnotation moduleName ret
renderType moduleName (ConstructorType gens args ret) = 
  keyword "new" <>
  sp <>
  foldMap (angles <<< intercalate (comma <> sp) <<< map (renderTypeParameter moduleName)) gens <>
  (parens $ intercalate (comma <> sp) $ map (renderParameter moduleName) args) <>
  renderTypeAnnotation moduleName ret
  
renderTypeBody :: [String] -> ([Html] -> Html) -> TypeBody -> Html
renderTypeBody moduleName wrap (TypeBody members) = wrap $ map (renderTypeMember moduleName) members

renderTypeName :: [String] -> TypeName -> Html
renderTypeName moduleName (TypeName Nothing name) =
  linkToBookmark (moduleName ++ [name]) $ ident name
renderTypeName _ (TypeName (Just m@(ModuleName moduleName)) name) =
  renderModuleName m <>
  syntax "." <>
  linkToBookmark (moduleName ++ [name]) (ident name)
  
renderModuleName :: ModuleName -> Html
renderModuleName (ModuleName parts) = linkToBookmark parts $ ident $ intercalate "." parts

renderParameter :: [String] -> Parameter -> Html
renderParameter moduleName (RequiredOrOptionalParameter publicOrPrivate name opt ty) =
  foldMap renderPublicOrPrivate publicOrPrivate <>
  ident name <>
  foldMap renderOptional opt <>
  foldMap (renderTypeAnnotation moduleName) ty
renderParameter moduleName (RestParameter name ty) =
  syntax "..." <>
  ident name <>
  foldMap (renderTypeAnnotation moduleName) ty
  
renderTypeAnnotation :: [String] -> Type -> Html
renderTypeAnnotation moduleName ty =
  syntax ":" <>
  sp <>
  renderType moduleName ty
  
renderTypeMember :: [String] -> (Tuple (Maybe Comment) TypeMember) -> Html
renderTypeMember moduleName (Tuple com mem) =
  (span $ code $ renderTypeMember' mem) <>
  foldMap renderComment com
  where
  renderTypeMember' (PropertySignature name opt ty) =
    ident name <>
    foldMap renderOptional opt <>
    colon <>
    sp <>
    foldMap (renderType moduleName) ty
  renderTypeMember' (MethodSignature name opt prt) =
    ident name <>
    foldMap renderOptional opt <>
    renderParameterListAndReturnType moduleName prt
  renderTypeMember' (CallSignature prt) = renderParameterListAndReturnType moduleName prt
  renderTypeMember' (ConstructSignature gens args ret) =
    foldMap (angles <<< intercalate (comma <> sp) <<< map (renderTypeParameter moduleName)) gens <>
    (parens $ intercalate (comma <> sp) $ map (renderParameter moduleName) args) <>
    foldMap (renderTypeAnnotation moduleName) ret
  renderTypeMember' (TypeIndexSignature index) = renderIndexSignature moduleName index
  
renderParameterListAndReturnType :: [String] -> ParameterListAndReturnType -> Html
renderParameterListAndReturnType moduleName (ParameterListAndReturnType gens args ret) =
  foldMap (angles <<< intercalate comma <<< map (renderTypeParameter moduleName)) gens <>
  (parens $ intercalate (comma <> sp) $ map (renderParameter moduleName) args) <>
  foldMap (renderTypeAnnotation moduleName) ret

renderIndexSignature :: [String] -> IndexSignature -> Html
renderIndexSignature moduleName (IndexSignature name key ret) =
  squares
    (ident name <>
    colon <>
    renderStringOrNumber key) <>
  renderTypeAnnotation moduleName ret
  
-- Render document

ifM :: Boolean -> Html -> Html
ifM false _ = mempty
ifM true  x = x

generateDocument :: [DeclarationElement] -> Html
generateDocument ds =
  let interfaces = topLevelInterfaces ds
      interfacesHtml = 
        ifM (not (null interfaces)) $
          header 1 (text "Interfaces") <>
          foldMap (renderInterface [] 2) interfaces
      declarations = ambientDeclarations ds
      declarationsHtml = 
        ifM (not (null declarations)) $
          header 1 (text "Declarations") <>
          renderAmbientDeclarations [] 2 declarations
      content = interfacesHtml <> declarationsHtml
  in html $ mconcat [ head $ link ! rel "stylesheet" ! href "style.css" $ mempty
                    , body content
                    ]
  