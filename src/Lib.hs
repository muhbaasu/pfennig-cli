{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}

module Lib where

import qualified Data.ByteString                      as BS
import           Data.Monoid                          ((<>))
import qualified Data.Serialize                       as S
import qualified Data.Serialize.Get                   as SG
import qualified Data.Serialize.Put                   as SP
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Time.Calendar                   as Cal
import qualified Data.Time.Format                     as Cal
import           GHC.Generics                         (Generic (..))
import qualified Options.Applicative                  as OA
import qualified Options.Applicative.Builder.Internal as OA
import qualified Pipes                                as P
import qualified Pipes.ByteString                     as PBS
import qualified Pipes.Safe                           as P
import qualified System.IO                            as IO

data Command =
    Add AddOptions
  | Modify ModifyOptions
  | Delete DeleteOptions
  | Show ShowOptions
  deriving (Show)

data Tag = Tag
  { _tagName :: SerializableText
  } deriving (Show, Eq, Generic)

instance Read Tag where
  readsPrec _ t = [(Tag . SerializableText $ T.pack t, "")]

newtype Amount = Amount
  { unAmount :: Rational
  } deriving (Show, Eq, Enum, Fractional, Num, Ord, Real, RealFrac, Generic)

instance Read Amount where
  readsPrec _ t = [((Amount . toRational . (read :: String -> Double)) t, "")]

data AddOptions = AddOptions
  { _addOptDate   :: Maybe Cal.Day
  , _addOptAmount :: Amount
  , _addOptTags   :: [Tag]
  } deriving (Show)

data ModifyOptions = ModifyOptions
  { _modOptId     :: ExpenseId
  , _modOptDate   :: Maybe Cal.Day
  , _modOptAmount :: Maybe Amount
  , _modOptTags   :: Maybe [Tag]
  } deriving (Show)

data DeleteOptions = DeleteOptions
  { _delOptId :: ExpenseId
  } deriving (Show)

data ShowOptions = ShowOptions
  {
  } deriving (Show)

programInfo :: OA.InfoMod a
programInfo = OA.fullDesc <>
  OA.progDesc "Pfennig CLI v0.0.1" <>
  OA.header "Pfennig CLI -- Manage your expenses using simple tags"

programOpts :: OA.Parser Command
programOpts = OA.hsubparser
  (OA.command "add"
   (OA.info addOptions $ OA.progDesc "Add an expense") <>
   OA.command "modify"
   (OA.info modifyOptions $ OA.progDesc "Modify an expense") <>
   OA.command "delete"
   (OA.info deleteOptions $ OA.progDesc "Delete an expense") <>
   OA.command "show"
   (OA.info showOptions $ OA.progDesc "Show recorded expenses"))

addOptions :: OA.Parser Command
addOptions = Add <$>
  (AddOptions <$>
   dayOption <*>
   amountArgument <*>
   tagArguments)

modifyOptions :: OA.Parser Command
modifyOptions = Modify <$>
  (ModifyOptions <$>
   expenseIdArgument <*>
   dayOption <*>
   amountOption <*>
   tagOptions)

deleteOptions :: OA.Parser Command
deleteOptions = Delete <$>
  (DeleteOptions <$>
   expenseIdArgument)

showOptions :: OA.Parser Command
showOptions = pure $ Show ShowOptions

expenseIdArgument :: OA.Parser ExpenseId
expenseIdArgument = OA.argument OA.auto expenseIdMods

dayOption :: OA.Parser (Maybe Cal.Day)
dayOption = (parseDay =<<) <$> OA.optional
  (OA.strOption
   (dayMods <>
    OA.long "date" <>
    OA.short 'd'))

amountOption :: OA.Parser (Maybe Amount)
amountOption = OA.optional $ OA.option OA.auto
  (amountMods <>
   OA.long "amount" <>
   OA.short 'a')

amountArgument :: OA.Parser Amount
amountArgument = OA.argument OA.auto amountMods

tagOptions :: OA.Parser (Maybe [Tag])
tagOptions = OA.optional $ OA.some $ OA.option OA.auto
  (tagsMods <>
   OA.long "tags" <>
   OA.short 't')

tagArguments :: OA.Parser [Tag]
tagArguments = OA.some $ OA.argument OA.auto tagsMods

expenseIdMods :: OA.HasMetavar f => OA.Mod f ExpenseId
expenseIdMods =
  OA.metavar "ID" <>
  OA.help "ID of the expense to modify"

dayMods :: OA.HasMetavar f => OA.Mod f String
dayMods =
  OA.metavar "DATE" <>
  OA.help "Date of the expense"

amountMods :: OA.HasMetavar f => OA.Mod f Amount
amountMods =
  OA.metavar "AMOUNT" <>
  OA.help "Amount of the expense"

tagsMods :: OA.HasMetavar f => OA.Mod f Tag
tagsMods =
  OA.metavar "TAGS" <>
  OA.help "Tags for the given expense"

parseDay :: String -> Maybe Cal.Day
parseDay = Cal.parseTimeM True Cal.defaultTimeLocale "%d.%m.%Y"

newtype ExpenseId = ExpenseId
  { unExpenseId :: Integer
  } deriving (Show, Eq, Generic)

instance Read ExpenseId where
  readsPrec _ s = [((ExpenseId . read) s, "")]

interpret :: Monad m => Command -> m ()
interpret (Add opts) = return ()
interpret (Modify opts) = return ()
interpret (Delete opts) = return ()
interpret (Show opts) = return ()

data Event =
    CreateExpense ExpenseCreation
  | ModifyExpense ExpenseModification
  | DeleteExpense ExpenseDeletion
  deriving (Show, Generic)

instance S.Serialize Event
instance S.Serialize ExpenseId
instance S.Serialize Amount
instance S.Serialize Tag

newtype SerializableDay = SerializableDay { unSerializableDay :: Cal.Day }
                          deriving (Show, Eq)

instance S.Serialize SerializableDay where
  put = S.put . Cal.toModifiedJulianDay . unSerializableDay
  get = SerializableDay . Cal.ModifiedJulianDay <$> S.get

newtype SerializableText = SerializableText { unSerializableText :: T.Text }
                           deriving (Show, Eq)

instance S.Serialize SerializableText where
  put t = do
    let bs = TE.encodeUtf8 $ unSerializableText t
    let l = fromIntegral $ BS.length bs
    SP.putWord64le l
    SP.putByteString bs
    return ()
  get = do
    l <- fromIntegral <$> SG.getWord64le
    SerializableText . TE.decodeUtf8 <$> SG.getByteString l

data ExpenseCreation = ExpenseCreation
  { _createId     :: ExpenseId
  , _createDate   :: SerializableDay
  , _createAmount :: Amount
  , _createTags   :: [Tag]
  } deriving (Show, Generic)

instance S.Serialize ExpenseCreation

data ExpenseModification = ExpenseModification
  { _modifyId     :: ExpenseId
  , _modifyDate   :: Maybe SerializableDay
  , _modifyAmount :: Maybe Amount
  , _modifyTags   :: Maybe [Tag]
  } deriving (Show, Generic)

instance S.Serialize ExpenseModification

data ExpenseDeletion = ExpenseDeletion
  { _deleteId :: ExpenseId
  } deriving (Show, Generic)

instance S.Serialize ExpenseDeletion

data Expense = Expense
  { _expenseId :: ExpenseId
  } deriving (Show)

newtype SizeTagged a = SizeTagged { unSizeTagged :: a } deriving (Show)

instance S.Serialize a => S.Serialize (SizeTagged a) where
  put s = SP.putNested (SP.putWord64le . fromIntegral) (S.put $ unSizeTagged s)
  get = SizeTagged <$> SG.getNested (fromIntegral <$> SG.getWord64le) S.get
