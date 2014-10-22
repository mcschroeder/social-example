module TX.MemoryOnly (MemoryOnly, newDatabase) where

import TX

data MemoryOnly

newDatabase :: d -> IO (DatabaseHandle MemoryOnly d)
newDatabase d =
    return DatabaseHandle { database   = d
                          , storage    = undefined
                          , serializer = const $ return ()
                          }
