module Helpers where

import Graph

import Control.Monad.State

getLink :: NodeId -> PortId -> NetState (Maybe Link)
getLink n p = do
    g <- get
    return (lookupPort n p g)

linkEnd :: Link -> (NodeId, PortId)
linkEnd (Link n p) = (n, p)

connectLinks :: Maybe Link -> Maybe Link -> NetState ()
connectLinks (Just l1) (Just l2) = link (linkEnd l1) (linkEnd l2)
connectLinks _ _  = return ()

linkTo :: (NodeId, PortId) -> Maybe Link -> NetState ()
linkTo port (Just l) = link port (linkEnd l)
linkTo _    Nothing  = return ()
