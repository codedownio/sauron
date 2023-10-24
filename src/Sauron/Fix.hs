
module Sauron.Fix (
  fixTree
  , unFixTree
  ) where

import Relude
import Sauron.Types


fixTree :: Node context -> STM (NodeFixed context)
fixTree node = undefined
  -- status <- readTVar runTreeStatus
  -- logs <- readTVar runTreeLogs
  -- toggled <- readTVar runTreeToggled
  -- open <- readTVar runTreeOpen

  -- let common' = RunNodeCommonWithStatus {
  --       runTreeStatus = status
  --       , runTreeLogs = logs
  --       , runTreeToggled = toggled
  --       , runTreeOpen = open
  --       , ..
  --       }

  -- case node of
  --   RunNodeBefore {..} -> do
  --     children <- mapM fixRunTree runNodeChildren
  --     return $ RunNodeBefore { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeAfter {..} -> do
  --     children <- mapM fixRunTree runNodeChildren
  --     return $ RunNodeAfter { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeIntroduce {..} -> do
  --     children <- mapM fixRunTree runNodeChildrenAugmented
  --     return $ RunNodeIntroduce { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
  --   RunNodeIntroduceWith {..} -> do
  --     children <- mapM fixRunTree runNodeChildrenAugmented
  --     return $ RunNodeIntroduceWith { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
  --   RunNodeAround {..} -> do
  --     children <- mapM fixRunTree runNodeChildren
  --     return $ RunNodeAround { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeDescribe {..} -> do
  --     children <- mapM fixRunTree runNodeChildren
  --     return $ RunNodeDescribe { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeParallel {..} -> do
  --     children <- mapM fixRunTree runNodeChildren
  --     return $ RunNodeParallel { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeIt {..} -> do
  --     return $ RunNodeIt { runNodeCommon=common', .. }

unFixTree :: NodeFixed context -> STM (Node context)
unFixTree node = undefined
  -- status <- newTVar runTreeStatus
  -- logs <- newTVar runTreeLogs
  -- toggled <- newTVar runTreeToggled
  -- open <- newTVar runTreeOpen

  -- let common' = RunNodeCommonWithStatus {
  --       runTreeStatus = status
  --       , runTreeLogs = logs
  --       , runTreeToggled = toggled
  --       , runTreeOpen = open
  --       , ..
  --       }

  -- case node of
  --   RunNodeBefore {..} -> do
  --     children <- mapM unFixRunTree runNodeChildren
  --     return $ RunNodeBefore { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeAfter {..} -> do
  --     children <- mapM unFixRunTree runNodeChildren
  --     return $ RunNodeAfter { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeIntroduce {..} -> do
  --     children <- mapM unFixRunTree runNodeChildrenAugmented
  --     return $ RunNodeIntroduce { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
  --   RunNodeIntroduceWith {..} -> do
  --     children <- mapM unFixRunTree runNodeChildrenAugmented
  --     return $ RunNodeIntroduceWith { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
  --   RunNodeAround {..} -> do
  --     children <- mapM unFixRunTree runNodeChildren
  --     return $ RunNodeAround { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeDescribe {..} -> do
  --     children <- mapM unFixRunTree runNodeChildren
  --     return $ RunNodeDescribe { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeParallel {..} -> do
  --     children <- mapM unFixRunTree runNodeChildren
  --     return $ RunNodeParallel { runNodeCommon=common', runNodeChildren=children, .. }
  --   RunNodeIt {..} -> do
  --     return $ RunNodeIt { runNodeCommon=common', .. }
