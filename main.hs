module Main (Main.main) where
import Control.Monad (sequence_)
import qualified Data.ByteString.Char8 (concat)
import qualified Data.Text as Text (pack,unpack,split,strip) 
import Data.Convertible.Base
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Database.HDBC
import Database.HDBC.Sqlite3
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums

extractByteString (SqlByteString bs) = bs

fields = ["BookID","Title", "Edition", "Author"]
textFields = drop 1 fields

modelFields = 
    foldr1 (++) ((map (++ " TEXT, ") (take ((length textFields)-1) textFields)) 
        ++ (map (++ " TEXT ") (drop ((length textFields)-1) textFields)))

getField :: IConnection connection => connection -> String -> String -> IO(Maybe Int)
getField connection tableName field = do
   tableCols <- getTableCols connection tableName
   return (elemIndex field tableCols)

getFields :: IConnection connection => connection -> String -> IO [Int]
getFields connection tableName =
    (mapM (getField connection tableName) textFields) >>= (\f -> return (map fromJust f))


createRowFilter :: Int -> ([SqlValue] -> [AttrOp CellRendererText])
createRowFilter index = (\row -> [cellText := (show (extractByteString (row !! index)))])

addColumn :: (TreeViewClass view, TreeModelClass (model row), CellRendererClass renderer, 
    TypedTreeModelClass model) 
    => view -> renderer -> model row -> String -> (row -> [AttrOp renderer]) -> IO()
addColumn view renderer model name func = do
    column <- treeViewColumnNew
    treeViewColumnSetTitle column name
    treeViewColumnPackStart column renderer True
    cellLayoutSetAttributes column renderer model func
    treeViewAppendColumn view column
    return ()

getTableCols connection tableName = do
    getSchema <- prepare connection ("SELECT sql FROM sqlite_master WHERE type='table' AND name='"++tableName++"';")
    execute getSchema []
    cols <- fetchRow getSchema
    sqlString <- return (map extractByteString (fromJust cols))
    str <- return (Data.ByteString.Char8.concat sqlString)
    tableVars <- return (takeWhile (/= ')') (drop 1 (dropWhile (/= '(') (show str))))
    tableCols <- return (map (Text.split (== ' ')) (map Text.strip (Text.split (== ',') (Text.pack(tableVars)))))
    return (map (\a -> a !! 0) (map (map Text.unpack) tableCols))

getMissingColumns connection tableName = do
    columns <- getTableCols connection tableName
    return (filter (\elem -> notElem elem columns) fields)

populateModel model connection = do
    create <- prepare connection ("CREATE TABLE IF NOT EXISTS Books (BookID " 
        ++ "INTEGER PRIMARY KEY, " ++ modelFields ++ ")")
    execute create []
    missingColumns <- getMissingColumns connection "Books"
    alterStatements <- return $ map (\m -> "ALTER TABLE Books ADD COLUMN " ++ m ++ " DEFAULT ''") missingColumns
    alters <- (mapM (prepare connection) alterStatements)
    mapM_ (\alt -> execute alt [])  alters
    commit connection

updateModel model connection = do
    listStoreClear model
    allBooks model connection
    return ()

allBooks :: IConnection conn => ListStore [SqlValue] -> conn -> IO [[SqlValue]]
allBooks model connection = do
    select <- prepare connection "SELECT * FROM Books;"
    execute select []
    books <- fetchAllRows select
    mapM_ (listStoreAppend model) (map (map fromSql) books)
    return books

insertBook :: IConnection conn => conn -> [String] -> IO ()
insertBook connection book = do
    sqlFields <- return ((map (++ ",") (take ((length textFields)-1) textFields))++(drop ((length textFields)-1) textFields))
    sqlCols <- return (foldr (++) "" sqlFields)
    sqlVals <- return ((foldr (++) "" (take ((length sqlFields)-1) (repeat "?,")))++"?")
    insert <- prepare connection ("INSERT INTO Books ("++sqlCols
        ++") Values ("++sqlVals++")")
    execute insert (map toSql book)
    commit connection
    return ()

addBook model connection entries = do
    book <- mapM (\x -> entryGetText (snd x)) entries
    insertBook connection book
    updateModel model connection
    return ()

addEntry :: BoxClass box => box -> String -> String -> IO (String, Entry)
addEntry box entryText fieldName = do
    hbox <- hBoxNew False 0
    label <- labelNew (Just fieldName)
    boxPackStart hbox label PackNatural 0
    entry <- entryNew
    entrySetText entry entryText
    boxPackStart hbox entry PackNatural 0
    boxPackStart box hbox PackNatural 0
    return (fieldName, entry)

addBookPopup model connection = do
    popup <- windowNew
    vbox <- vBoxNew False 0
    entryFields <- mapM (addEntry vbox "") textFields
    button <- buttonNewWithLabel "Add Book"
    on button buttonActivated (addBook model connection entryFields)
    boxPackStart vbox button PackNatural 0
    button <- buttonNewWithLabel "Cancel"
    on button buttonActivated (widgetDestroy popup)
    boxPackStart vbox button PackNatural 0
    containerAdd popup vbox
    widgetShowAll popup 

removeBook :: IConnection conn => conn -> SqlValue -> IO ()
removeBook connection bookKey = do
    del <- prepare connection "DELETE FROM Books WHERE BookID=?"
    execute del (map toSql [bookKey])
    commit connection


delBook model view connection = do
    selection <- treeViewGetSelection view
    selected <- treeSelectionGetSelected selection
    book <- listStoreGetValue model (listStoreIterToIndex (fromJust selected))
    removeBook connection (head book)
    updateModel model connection
    return ()

updateBook :: IConnection conn => ListStore [SqlValue] -> conn -> SqlValue -> [(SqlValue,SqlValue)] -> IO ()
updateBook model connection bookID newValues = do
    -- column field must be known at time of prepare, so col placed into string
    updates <- return (map (\ (col, value) -> 
        (prepare connection ("UPDATE Books SET "++(fromSql col)++"=? WHERE BookID=?")) >>=
        (\ u -> execute u [value,bookID])) newValues)
    sequence_ updates
    commit connection
    updateModel model connection
    return ()

-- specifies return type of entryGetText as IO String
-- so compiler can deduce type
getText :: (a, Entry) -> IO String
getText entry  = entryGetText (snd entry)

editBook model view connection entries = do
    selection <- treeViewGetSelection view
    selected <- treeSelectionGetSelected selection
    book <- listStoreGetValue model (listStoreIterToIndex (fromJust selected))
    bookID <- return (head book)
    updateVals <- ((mapM getText entries) >>= (\x -> return (map toSql x)))
    updateBook model connection bookID  (zip (map toSql textFields) updateVals)
    return ()

editBookPopup :: (TreeViewClass view, IConnection conn) =>
    ListStore [SqlValue] -> view -> conn -> IO ()
editBookPopup model view connection = do
    popup <- windowNew
    vbox <- vBoxNew False 0
    selection <- treeViewGetSelection view
    selected <- treeSelectionGetSelected selection
    book <- listStoreGetValue model (listStoreIterToIndex (fromJust selected))
    fieldIndexes <- getFields connection "Books"
    entryFields <-  sequence (zipWith (\ entryWithVal fieldName -> entryWithVal fieldName) 
        (map (\ index -> addEntry vbox (fromSql (book !! index))) fieldIndexes)
        textFields)
    button <- buttonNewWithLabel "Update Book"
    on button buttonActivated (editBook model view connection entryFields >> 
        widgetDestroy popup)
    boxPackStart vbox button PackNatural 0
    button <- buttonNewWithLabel "Cancel"
    on button buttonActivated (widgetDestroy popup)
    boxPackStart vbox button PackNatural 0
    containerAdd popup vbox
    widgetShowAll popup

main :: IO ()
main = do
    initGUI
    window <- windowNew
    bookList <- scrolledWindowNew (Nothing) (Nothing)
    scrolledWindowSetPolicy bookList PolicyNever PolicyAutomatic
    on window objectDestroy mainQuit
    model <- listStoreNew ([] :: [[SqlValue]])
    view <- treeViewNewWithModel model
    vbox <- vBoxNew False 0

    connection <- connectSqlite3 "books.sqlite"
    populateModel model connection
    allBooks model connection

    renderer <- cellRendererTextNew

    columns <- return (map 
        (addColumn view renderer model) textFields)
    fieldIndexes <- sequence (map (getField connection "Books") textFields)
    filters <- return (map createRowFilter (map fromJust fieldIndexes))
    sequence_ (zipWith (\ c f -> c f) (columns) (filters))

    containerAdd bookList view
    containerAdd vbox bookList

    buttonRow <- hBoxNew False 0
    button <- buttonNewWithLabel "Add"
    on button buttonActivated (addBookPopup model connection)
    boxPackStart buttonRow button PackNatural 0
    button <- buttonNewWithLabel "Delete"
    on button buttonActivated (delBook model view connection)
    boxPackStart buttonRow button PackNatural 0
    button <- buttonNewWithLabel "Edit"
    on button buttonActivated (editBookPopup model view connection)
    boxPackStart buttonRow button PackNatural 0
    boxPackStart vbox buttonRow PackNatural 0

    containerAdd window vbox
    widgetShowAll window
    mainGUI
    Database.HDBC.disconnect connection

