module Language.TypeScript.Types where

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Monoid

import qualified Data.Map as M

type Location = Tuple Number Number

infixl 5 <->

(<->) :: forall a. (Show a) => String -> a -> String
(<->) s a = s <> " " <> show a

newtype Comment = Comment [String]

instance semigroupComment :: Semigroup Comment where
  (<>) (Comment ss1) (Comment ss2) = Comment (ss1 <> ss2)

instance showComment :: Show Comment where
  show (Comment s) = "(Comment" <-> s <> ")"

data DeclarationElement
  = InterfaceDeclaration (Maybe Exported) Interface
  | ImportDeclaration (Maybe Exported) String EntityName
  | ExportDeclaration String
  | ExternalImportDeclaration (Maybe Exported) String String
  | AmbientDeclaration (Maybe Comment) (Maybe Exported) Ambient

instance showDeclarationElement :: Show DeclarationElement where
  show (InterfaceDeclaration exp int) = "(InterfaceDeclaration" <-> exp <-> int <> ")"
  show (ImportDeclaration exp s ent) = "(ImportDeclaration" <-> exp <-> s <-> ent <> ")"
  show (ExportDeclaration s) = "(ExportDeclaration" <-> s <> ")"
  show (ExternalImportDeclaration exp s1 s2) = "(ExternalImportDeclaration" <-> exp <-> s1 <-> s2 <> ")"
  show (AmbientDeclaration com exp amb) = "(AmbientDeclaration" <-> com <-> exp <-> amb <> ")"

data Exported = Exported

instance showExported :: Show Exported where
  show _ = "Exported"

data EntityName = EntityName (Maybe ModuleName) String

instance showEntityName :: Show EntityName where
  show (EntityName mn s) = "(EntityName" <-> mn <-> s <> ")"

data Interface = Interface (Maybe Comment) String (Maybe [TypeParameter]) (Maybe [TypeRef]) TypeBody

instance showInterface :: Show Interface where
  show (Interface com s tps trs tb) = "(Interface " <-> com <-> s <-> tps <-> trs <-> tb <> ")"

data Ambient
  = AmbientVariableDeclaration (Maybe Comment) String (Maybe Type)
  | AmbientFunctionDeclaration (Maybe Comment) String ParameterListAndReturnType
  | AmbientClassDeclaration (Maybe Comment) String (Maybe [TypeParameter]) (Maybe [TypeRef]) (Maybe [TypeRef]) [Tuple (Maybe Comment) AmbientClassBodyElement]
  | AmbientInterfaceDeclaration Interface
  | AmbientEnumDeclaration (Maybe Comment) String [Tuple String (Maybe Number)]
  | AmbientModuleDeclaration (Maybe Comment) [String] [Ambient]
  | AmbientExternalModuleDeclaration (Maybe Comment) String [AmbientExternalModuleElement]

instance showAmbient :: Show Ambient where
  show (AmbientVariableDeclaration com s t) =              "(AmbientVariableDeclaration" <-> com <-> s <-> t <> ")"
  show (AmbientFunctionDeclaration com s plrt) =           "(AmbientFunctionDeclaration" <-> com <-> s <-> plrt <> ")"
  show (AmbientClassDeclaration com s tps trs1 trs2 els) = "(AmbientClassDeclaration" <-> com <-> s <-> tps <-> trs1 <-> trs2 <-> els <> ")"
  show (AmbientInterfaceDeclaration i) =                   "(AmbientInterfaceDeclaration" <-> i <> ")"
  show (AmbientEnumDeclaration com es els) =               "(AmbientEnumDeclaration" <-> com <-> es <-> els <> ")"
  show (AmbientModuleDeclaration com s els) =              "(AmbientModuleDeclaration" <-> com <-> s <-> els <> ")"
  show (AmbientExternalModuleDeclaration com s els) =      "(AmbientExternalModuleDeclaration" <-> com <-> s <-> els <> ")"

data AmbientExternalModuleElement
  = AmbientModuleElement Ambient
  | ExportAssignment String
  | AmbientModuleExternalImportDeclaration (Maybe Exported) String String

instance showAmbientExternalModuleElement :: Show AmbientExternalModuleElement where
  show (AmbientModuleElement a) =                         "(AmbientModuleElement" <-> a <> ")"
  show (ExportAssignment s) =                             "(ExportAssignment" <-> s <> ")"
  show (AmbientModuleExternalImportDeclaration e s1 s2) = "(AmbientModuleExternalImportDeclaration" <-> e <-> s1 <-> s2 <> ")"

data TypeRef = TypeRef TypeName (Maybe [Type])

instance showTypeRef :: Show TypeRef where
  show (TypeRef tn args) = "(TypeRef" <-> tn <-> args <> ")"

data AmbientClassBodyElement
  = AmbientConstructorDeclaration [Parameter]
  | AmbientMemberDeclaration (Maybe PublicOrPrivate) (Maybe Static) String (Either (Maybe Type) ParameterListAndReturnType)
  | AmbientIndexSignature IndexSignature

instance showAmbientClassBodyElement :: Show AmbientClassBodyElement where
  show (AmbientConstructorDeclaration ps) = "(AmbientConstructorDeclaration" <-> ps <> ")"
  show (AmbientMemberDeclaration pop st s e) = "(AmbientMemberDeclaration" <-> pop <-> st <-> s <-> e <> ")"
  show (AmbientIndexSignature is) = "(AmbientIndexSignature" <-> is <> ")"

data Static = Static 

instance showStatic :: Show Static where
  show _ = "Static"

data Optional = Optional

instance showOptional :: Show Optional where
  show _ = "Optional"

data TypeBody = TypeBody [Tuple (Maybe Comment) TypeMember]

instance showTypeBody :: Show TypeBody where
  show (TypeBody ts) = "(TypeBody" <-> ts <> ")"

data TypeMember
  = PropertySignature String (Maybe Optional) (Maybe Type)
  | CallSignature ParameterListAndReturnType
  | ConstructSignature (Maybe [TypeParameter]) [Parameter] (Maybe Type)
  | TypeIndexSignature IndexSignature
  | MethodSignature String (Maybe Optional) ParameterListAndReturnType

instance showTypeMember :: Show TypeMember where
  show (PropertySignature s o t) = "(PropertySignature" <-> s <-> o <-> t <> ")"
  show (CallSignature plrt) = "(CallSignature" <-> plrt <> ")"
  show (ConstructSignature ts ps t) = "(ConstructSignature" <-> ts <-> ps <-> t <> ")"
  show (TypeIndexSignature is) = "(TypeIndexSignature" <-> is <> ")"
  show (MethodSignature s o plrt) = "(MethodSignature" <-> s <-> o <-> plrt <> ")"

data IndexSignature = IndexSignature String StringOrNumber Type

instance showIndexSignature :: Show IndexSignature where
  show (IndexSignature s son t) = "(IndexSignature" <-> s <-> son <-> t <> ")"

data ParameterListAndReturnType = ParameterListAndReturnType (Maybe [TypeParameter]) [Parameter] (Maybe Type) 

instance showParameterListAndReturnType :: Show ParameterListAndReturnType where
  show (ParameterListAndReturnType tps ps rt) = "(ParameterListAndReturnType" <-> tps <-> ps <-> rt <> ")" 

data Parameter
  = RequiredOrOptionalParameter (Maybe PublicOrPrivate) String (Maybe Optional) (Maybe Type)
  | RestParameter String (Maybe Type)

instance showParameter :: Show Parameter where
  show (RequiredOrOptionalParameter pop s o t) = "(RequiredOrOptionalParameter" <-> pop <-> s <-> o <-> t <> ")"
  show (RestParameter s t) = "(RestParameter" <-> s <-> t <> ")"

data StringOrNumber = String | Number

instance showStringOrNumber :: Show StringOrNumber where
  show String = "String"
  show Number = "Number"

data PublicOrPrivate = Public | Private

instance showPublicOrPrivate :: Show PublicOrPrivate where
  show Public = "Public"
  show Private = "Private"

data TypeParameter = TypeParameter String (Maybe Type)

instance showTypeParameter :: Show TypeParameter where
  show (TypeParameter s t) = "(TypeParameter" <-> s <-> t <> ")"

data Type
  = Predefined PredefinedType
  | TypeReference TypeRef
  | ObjectType TypeBody
  | ArrayType Type
  | FunctionType (Maybe [TypeParameter]) [Parameter] Type
  | ConstructorType (Maybe [TypeParameter]) [Parameter] Type

instance showType :: Show Type where
  show (Predefined pt) = "(Predefined" <-> pt <> ")"
  show (TypeReference tr) = "(TypeReference" <-> tr <> ")"
  show (ObjectType tb) = "(ObjectType" <-> tb <> ")"
  show (ArrayType t) = "(ArrayType" <-> t <> ")"
  show (FunctionType tps ps t) = "(FunctionType" <-> tps <-> ps <-> t <> ")"
  show (ConstructorType tps ps t) = "(ConstructorType" <-> tps <-> ps <-> t <> ")"

data TypeName = TypeName (Maybe ModuleName) String

instance showTypeName :: Show TypeName where
  show (TypeName mn s) = "(TypeName" <-> mn <-> s <> ")"

data ModuleName = ModuleName [String]

instance showModuleName :: Show ModuleName where
  show (ModuleName ss) = "(ModuleName" <-> ss <> ")"

data PredefinedType
  = AnyType
  | NumberType
  | BooleanType
  | StringType
  | VoidType

instance showPredefinedType :: Show PredefinedType where
  show AnyType = "AnyType"
  show NumberType = "NumberType"
  show BooleanType = "BooleanType"
  show StringType = "StringType"
  show VoidType = "VoidType"


