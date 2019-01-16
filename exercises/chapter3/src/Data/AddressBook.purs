module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe, isJust)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findByAddress :: Address -> AddressBook -> Maybe Entry
findByAddress address = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address == address

isNameInBook :: String -> String -> AddressBook -> Boolean
isNameInBook firstName lastName = isJust <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy sameName
  where
    sameName :: Entry -> Entry -> Boolean
    sameName a b = (a.firstName == b.firstName) && (a.lastName == b.lastName)
