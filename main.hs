module Main (Main.main) where

import Data.Maybe (fromJust)
import Database.HDBC
import Database.HDBC.Sqlite3
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums

data Book = Book { bookID :: String,
    title :: String,
    description :: String } deriving (Show)

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
    create <- prepare connection ("CREATE TABLE IF NOT EXISTS Books (BookID INTEGER PRIMARY KEY,"
        ++ "Title TEXT, Description TEXT);")
    execute create []
    commit connection

updateModel model connection = do
    listStoreClear model
    allBooks model connection

allBooks model connection = do
    select <- prepare connection "SELECT * FROM Books;"
    execute select []
    books <- fetchAllRows select
    mapM_ (listStoreAppend model) 
        (map (\row -> Book (row !! 0) (row !! 1) (row !! 2)) (map (map fromSql) books))
    return books

insertBook :: IConnection conn => conn -> Book -> IO ()
insertBook connection book = do
    insert <- prepare connection "INSERT INTO Books (Title, Description) Values (?,?)"
    execute insert (map toSql [title book, description book])
    commit connection

addBook model connection entries = do
    title <- entryGetText (snd (head entries))
    desc <- entryGetText (snd (head (tail entries)))
    insertBook connection (Book "0" title desc)
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

removeBook connection bookID = do
    del <- prepare connection "DELETE FROM Books WHERE BookID=?"
    execute del (map toSql [bookID])
    commit connection

delBook model view connection = do
    selection <- treeViewGetSelection view
    selected <- treeSelectionGetSelected selection
    book <- listStoreGetValue model (listStoreIterToIndex (fromJust selected))
    removeBook connection (bookID book)
    updateModel model connection
    return ()

addBookPopup model connection = do
    popup <- windowNew
    vbox <- vBoxNew False 0
    fields <- mapM (addEntry vbox) ["Title","Description"]
    button <- buttonNewWithLabel "Add Book"
    on button buttonActivated (addBook model connection fields)
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
    model <- listStoreNew []
    view <- treeViewNewWithModel model
    vbox <- vBoxNew False 0

    c <- connectSqlite3 "books.sqlite"
    populateModel model c
    allBooks model c

    renderer <- cellRendererTextNew

    column <- treeViewColumnNew
    treeViewColumnSetVisible column False
    treeViewColumnPackStart column renderer False
    cellLayoutSetAttributes column renderer model (\row -> [cellText := bookID row])
    treeViewAppendColumn view column

    addColumn view renderer model "Title" (\row -> [cellText := title row])
    addColumn view renderer model "Description" (\row -> [cellText := description row])
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
