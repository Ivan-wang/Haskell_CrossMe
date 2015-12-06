module Status (
    Status(..),
    switch) where

data Status = Set | Unset | Unknown

instance Show Status where
    show Set = "Set"
    show Unset = "Unset"
    show Unknown = "Unknown"

instance Eq Status where
    Set == Set = True
    Set == _ = False
    _ == Set = False
    _ == _ = True

switch :: Status -> Status
switch Unknown = Set
switch Set = Unset
switch Unset = Unknown