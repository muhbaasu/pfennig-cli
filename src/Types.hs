{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}

module Types where

import           Data.Hashable      (Hashable)
import qualified Data.Serialize     as S
import qualified Data.Serialize.Get as SG
import qualified Data.Serialize.Put as SP
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Calendar as Cal
import           GHC.Generics       (Generic (..))

data Command =
    Add !AddOptions
  | Modify !ModifyOptions
  | Delete !DeleteOptions
  | Show !ShowOptions
  deriving (Show)

data Tag = Tag
  { _tagName :: !SerializableText
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
  , _addOptAmount :: !Amount
  , _addOptTags   :: ![Tag]
  } deriving (Show)

data ModifyOptions = ModifyOptions
  { _modOptId     :: !ExpenseId
  , _modOptDate   :: Maybe Cal.Day
  , _modOptAmount :: Maybe Amount
  , _modOptTags   :: Maybe [Tag]
  } deriving (Show)

data DeleteOptions = DeleteOptions
  { _delOptId :: !ExpenseId
  } deriving (Show)

data ShowOptions = ShowOptions
  {
  } deriving (Show)

data GlobalOptions = GlobalOptions
  { _globOptDb :: !FilePath
  } deriving (Show)

newtype ExpenseId = ExpenseId
  { unExpenseId :: Integer
  } deriving (Show, Eq, Generic, Hashable)

instance Read ExpenseId where
  readsPrec _ s = [((ExpenseId . read) s, "")]

data Event =
    CreateExpense !ExpenseCreation
  | ModifyExpense !ExpenseModification
  | DeleteExpense !ExpenseDeletion
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
  put t = SP.putNested (SP.putWord64le . fromIntegral)
    (S.put . TE.encodeUtf8 $ unSerializableText t)
  get = SerializableText <$>
    SG.getNested (fromIntegral <$> SG.getWord64le) (TE.decodeUtf8 <$> S.get)

data ExpenseCreation = ExpenseCreation
  { _createId     :: !ExpenseId
  , _createDate   :: !SerializableDay
  , _createAmount :: !Amount
  , _createTags   :: ![Tag]
  } deriving (Show, Generic)

instance S.Serialize ExpenseCreation

data ExpenseModification = ExpenseModification
  { _modifyId     :: !ExpenseId
  , _modifyDate   :: Maybe SerializableDay
  , _modifyAmount :: Maybe Amount
  , _modifyTags   :: Maybe [Tag]
  } deriving (Show, Generic)

instance S.Serialize ExpenseModification

data ExpenseDeletion = ExpenseDeletion
  { _deleteId :: !ExpenseId
  } deriving (Show, Generic)

instance S.Serialize ExpenseDeletion

data Expense = Expense
  { _expenseId     :: !ExpenseId
  , _expenseDate   :: !Cal.Day
  , _expenseAmount :: !Amount
  , _expenseTags   :: ![Tag]
  } deriving (Show)

newtype SizeTagged a = SizeTagged { unSizeTagged :: a } deriving (Show)

instance S.Serialize a => S.Serialize (SizeTagged a) where
  put s = SP.putNested (SP.putWord64le . fromIntegral) (S.put $ unSizeTagged s)
  get = SizeTagged <$> SG.getNested (fromIntegral <$> SG.getWord64le) S.get
