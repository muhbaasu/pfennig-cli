{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib where

import           Control.Monad                        (join)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader                 (ReaderT, ask)
import qualified Control.Monad.State.Strict           as State
import           Control.Monad.Trans.Class            (lift)
import           Data.Hashable                        (Hashable)
import qualified Data.HashMap.Strict                  as HM
import           Data.List                            (intersperse, sortOn)
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Data.Monoid                          ((<>))
import qualified Data.Serialize                       as S
import qualified Data.Serialize.Get                   as SG
import qualified Data.Serialize.Put                   as SP
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Time.Calendar                   as Cal
import qualified Data.Time.Format                     as Cal
import qualified Data.Time.LocalTime                  as Time
import           GHC.Generics                         (Generic (..))
import qualified Options.Applicative                  as OA
import qualified Options.Applicative.Builder.Internal as OA
import qualified Pipes                                as P
import qualified Pipes.ByteString                     as PBS
import qualified Pipes.Prelude                        as P
import qualified Pipes.Safe                           as P
import qualified System.IO                            as IO
import           Text.Printf                          (printf)

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

data GlobalOptions = GlobalOptions
  { _globOptDb :: FilePath
  } deriving (Show)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions { _globOptDb = "./db.bin" }

programInfo :: OA.InfoMod a
programInfo = OA.fullDesc <>
  OA.progDesc "Pfennig CLI v0.1.0.0" <>
  OA.header "Pfennig CLI -- Manage your expenses"

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
  } deriving (Show, Eq, Generic, Hashable)

instance Read ExpenseId where
  readsPrec _ s = [((ExpenseId . read) s, "")]

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
  put t = SP.putNested (SP.putWord64le . fromIntegral)
    (S.put . TE.encodeUtf8 $ unSerializableText t)
  get = SerializableText <$>
    SG.getNested (fromIntegral <$> SG.getWord64le) (TE.decodeUtf8 <$> S.get)

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
  { _expenseId     :: ExpenseId
  , _expenseDate   :: Cal.Day
  , _expenseAmount :: Amount
  , _expenseTags   :: [Tag]
  } deriving (Show)

formatExpense :: Expense -> String
formatExpense Expense {..} =
  "#" ++ formatId _expenseId ++ "\t" ++
  formatDate _expenseDate ++ "\t€ " ++
  formatAmount _expenseAmount ++ "\t\t" ++
  formatTags _expenseTags

formatId :: ExpenseId -> String
formatId = show . unExpenseId

formatDate :: Cal.Day -> String
formatDate = Cal.formatTime Cal.defaultTimeLocale "%d.%m.%Y"

formatAmount :: Amount -> String
formatAmount a = printf "%5.2f" (fromRational (unAmount a) :: Double)

formatTags :: [Tag] -> String
formatTags = join . intersperse ", " . map tagToString
  where tagToString = T.unpack . unSerializableText . _tagName

newtype SizeTagged a = SizeTagged { unSizeTagged :: a } deriving (Show)

instance S.Serialize a => S.Serialize (SizeTagged a) where
  put s = SP.putNested (SP.putWord64le . fromIntegral) (S.put $ unSizeTagged s)
  get = SizeTagged <$> SG.getNested (fromIntegral <$> SG.getWord64le) S.get

{-|
  File handling
-}

defaultExpenseId :: ExpenseId
defaultExpenseId = ExpenseId 0

incExpenseId :: ExpenseId -> ExpenseId
incExpenseId = ExpenseId . (+1) . unExpenseId

nextExpenseId :: [Event] -> ExpenseId
nextExpenseId =
  maybe defaultExpenseId incExpenseId . safeLast . mapMaybe createId
  where safeLast [] = Nothing
        safeLast s = return $ last s
        createId (CreateExpense opts) = Just $ _createId opts
        createId _ = Nothing

getEvents :: (P.MonadIO m, P.MonadMask m) => FilePath -> m [Event]
getEvents file = P.runSafeT . P.toListM $ fileReader file P.>-> decodeEvents

appendEvents :: (P.MonadIO m, P.MonadMask m) => FilePath -> [Event] -> m ()
appendEvents file events = P.runSafeT . P.runEffect $
  P.each events P.>-> encodeEvents P.>-> fileAppender file

fileReader :: (P.MonadIO m, P.MonadMask m) =>
              FilePath ->
              P.Producer PBS.ByteString (P.SafeT m) ()
fileReader file = P.bracket
  (liftIO $ IO.openBinaryFile file IO.ReadWriteMode)
  (liftIO . IO.hClose)
  PBS.fromHandle

fileAppender :: (P.MonadIO m, P.MonadMask m) =>
                FilePath ->
                P.Consumer PBS.ByteString (P.SafeT m) ()
fileAppender file = P.bracket
  (liftIO $ IO.openBinaryFile file IO.AppendMode)
  (liftIO . IO.hClose)
  PBS.toHandle

interpret :: (P.MonadIO m, P.MonadMask m) =>
             Command -> ReaderT GlobalOptions m ()
interpret (Add opts) = do
  file <- _globOptDb <$> ask
  events <- getEvents file
  let nextId = nextExpenseId events
  zonedTime <- liftIO Time.getZonedTime
  let today = Time.localDay . Time.zonedTimeToLocalTime $ zonedTime
  let expenseCreation = ExpenseCreation
        { _createId = nextId
        , _createDate = SerializableDay $ fromMaybe today $ _addOptDate opts
        , _createAmount = _addOptAmount opts
        , _createTags = _addOptTags opts }
  appendEvents file [CreateExpense expenseCreation]
interpret (Modify mo) = do
  file <- _globOptDb <$> ask
  events <- getEvents file
  let expenses = computeExpenses events
  let eid = _modOptId mo
  let idExists = any (\e -> _expenseId e == eid) expenses
  if idExists                   -- Check if any changes included at all
     then let expenseModification = ExpenseModification
                { _modifyId = eid
                , _modifyDate = SerializableDay <$> _modOptDate mo
                , _modifyAmount = _modOptAmount mo
                , _modifyTags = _modOptTags mo }
          in appendEvents file [ModifyExpense expenseModification]
    else do
      liftIO $ putStrLn $ "Couldn't find expense with ID #" ++ formatId eid
      return ()
interpret (Delete delOpt) = do
  file <- _globOptDb <$> ask
  events <- getEvents file
  let expenses = computeExpenses events
  let eid = _delOptId delOpt
  let idExists = any (\e -> _expenseId e == eid) expenses
  if idExists
     then let expenseDeletion = ExpenseDeletion { _deleteId = eid }
          in appendEvents file [DeleteExpense expenseDeletion]
    else do
      liftIO $ putStrLn $ "Couldn't find expense with ID #" ++ formatId eid
      return ()
interpret (Show _) = do
  cfg <- ask
  events <- getEvents $ _globOptDb cfg
  let expenses = computeExpenses events
  liftIO $ mapM_ (putStrLn . formatExpense) expenses

computeExpenses :: [Event] -> [Expense]
computeExpenses = sortOn _expenseDate . HM.elems . fst .
  foldl applyEvent (HM.empty, defaultExpenseId)

applyEvent :: (HM.HashMap ExpenseId Expense, ExpenseId) ->
              Event ->
              (HM.HashMap ExpenseId Expense, ExpenseId)
applyEvent (hm, eid) (CreateExpense ec) =
  let ex = Expense { _expenseId = eid
                   , _expenseDate = unSerializableDay $ _createDate ec
                   , _expenseAmount = _createAmount ec
                   , _expenseTags = _createTags ec }
      hm' = HM.insert eid ex hm
  in (hm', incExpenseId eid)
applyEvent acc@(hm, eid) (ModifyExpense me) =
  let lookupId = _modifyId me
      ex = HM.lookup lookupId hm
  in case ex of
    Nothing -> acc
    Just ex' ->
      let newDate = fromMaybe (_expenseDate ex')
            (unSerializableDay <$> _modifyDate me)
          newAmount = fromMaybe (_expenseAmount ex') (_modifyAmount me)
          newTags = fromMaybe (_expenseTags ex') (_modifyTags me)
          newEx = Expense { _expenseId = _expenseId ex'
                          , _expenseDate = newDate
                          , _expenseAmount = newAmount
                          , _expenseTags = newTags }
      in (HM.insert lookupId newEx hm, eid)
applyEvent acc@(hm, eid) (DeleteExpense de) =
  let lookupId = _deleteId de
  in if HM.member lookupId hm
     then (HM.delete lookupId hm, eid)
     else acc

-- | Serialize events
encodeEvents :: Monad m => P.Pipe Event PBS.ByteString m ()
encodeEvents = P.map S.encode

-- | Deserialize events incrementally; abort on any parse error
decodeEvents :: Monad m => P.Pipe PBS.ByteString Event m ()
decodeEvents = State.evalStateT go Nothing
  where go = do
          s <- State.get
          case s of
            Nothing -> do              -- No previous state available
              bs <- lift P.await
              State.put . Just . SG.runGetPartial S.get $ bs
              go
            Just r -> case r of        -- Previous state available
              SG.Fail _ _ -> return () -- Abort, parsing failed
              SG.Partial cont -> do    -- Request more input
                bs <- lift P.await
                State.put . Just . cont $ bs
                go
              SG.Done evt rest -> do   -- Parsed an event successfully
                lift $ P.yield evt
                State.put . Just . SG.runGetPartial S.get $ rest
                go
