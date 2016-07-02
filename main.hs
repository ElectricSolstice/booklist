module Main (Main.main) where
import Control.Monad (sequence_)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Database.HDBC
import Database.HDBC.Sqlite3
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums

fields = ["BookID","Title", "Description"]
textFields = drop 1 fields

modelFields = 
    foldr1 (++) ((map (++ " TEXT, ") (take ((length textFields)-1) textFields)) 
        ++ (map (++ " TEXT ") (drop ((length textFields)-1) textFields)))

getField field = elemIndex field fields

createRowFilter :: Int -> ([String] -> [AttrOp CellRendererText])
createRowFilter index = (\row -> [cellText := row !! index])

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

populateModel model connection = do
    create <- prepare connection ("CREATE TABLE IF NOT EXISTS Books (BookID " 
        ++ "INTEGER PRIMARY KEY, " ++ modelFields ++ ")")
    execute create []
    commit connection

updateModel model connection = do
    listStoreClear model
    allBooks model connection
    return ()

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

addEntry :: BoxClass box => box -> String -> IO (String, Entry)
addEntry box fieldName = do
    hbox <- hBoxNew False 0
    label <- labelNew (Just fieldName)
    boxPackStart hbox label PackNatural 0
    entry <- entryNew
    boxPackStart hbox entry PackNatural 0
    boxPackStart box hbox PackNatural 0
    return (fieldName, entry)

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

addBookPopup model connection = do
    popup <- windowNew
    vbox <- vBoxNew False 0
    entryFields <- mapM (addEntry vbox) textFields
    button <- buttonNewWithLabel "Add Book"
    on button buttonActivated (addBook model connection entryFields)
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
    model <- listStoreNew ([] :: [[String]])
    view <- treeViewNewWithModel model
    vbox <- vBoxNew False 0

    c <- connectSqlite3 "books.sqlite"
    populateModel model c
    allBooks model c

    renderer <- cellRendererTextNew

    columns <- return (map 
        (addColumn view renderer model) textFields)
    filters <- return (map createRowFilter [1..(length fields)-1])
    sequence_ (zipWith (\ c f -> c f) (columns) (filters))

    containerAdd bookList view
    containerAdd vbox bookList

    buttonRow <- hBoxNew False 0
    button <- buttonNewWithLabel "Add Book"
    on button buttonActivated (addBookPopup model c)
    boxPackStart buttonRow button PackNatural 0
    button <- buttonNewWithLabel "Delete Book"
    on button buttonActivated (delBook model view c)
    boxPackStart buttonRow button PackNatural 0
    boxPackStart vbox buttonRow PackNatural 0

    containerAdd window vbox
    widgetShowAll window
    mainGUI
    Database.HDBC.disconnect c

