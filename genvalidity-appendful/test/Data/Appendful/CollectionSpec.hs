{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Appendful.CollectionSpec
  ( spec,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Monad.State
import Data.Appendful.Collection
import Data.Data
import Data.GenValidity.Appendful.Collection ()
import Data.GenValidity.UUID ()
import Data.List
import qualified Data.Map.Strict as M
import Data.UUID
import Data.Word
import GHC.Generics (Generic)
import System.Random
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils
import Text.Colour

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  let yamlSchemaSpec :: forall a. (Typeable a, HasCodec a) => FilePath -> Spec
      yamlSchemaSpec filePath = do
        it ("outputs the same schema as before for " <> nameOf @a) $
          pureGoldenTextFile
            ("test_resources/collection/" <> filePath <> ".txt")
            (renderChunksText With24BitColours $ schemaChunksViaCodec @a)

  describe "ClientStore" $ do
    genValidSpec @(ClientStore ClientId Word8 Word8)
    jsonSpec @(ClientStore ClientId Word8 Word8)
    yamlSchemaSpec @(ClientStore ClientId Word8 Word8) "client"
  describe "SyncRequest" $ do
    genValidSpec @(SyncRequest ClientId Word8 Word8)
    jsonSpec @(SyncRequest ClientId Word8 Word8)
    yamlSchemaSpec @(SyncRequest ClientId Word8 Word8) "request"
  describe "SyncResponse" $ do
    genValidSpec @(SyncResponse ClientId Word8 Word8)
    jsonSpec @(SyncResponse ClientId Word8 Word8)
    yamlSchemaSpec @(SyncResponse ClientId Word8 Word8) "response"
  describe "ServerStore" $ do
    genValidSpec @(ServerStore Word8 Word8)
    jsonSpec @(ServerStore Word8 Word8)
    yamlSchemaSpec @(ServerStore Word8 Word8) "server"

  describe "emptyStore" $ it "is valid" $ shouldBeValid (emptyClientStore @Int @Int @Int)
  describe "storeSize" $ do
    it "does not crash" $ producesValid (storeSize @Int @Int @Int)
    specify "adding an item makes the store bigger" $
      forAllValid $
        \store ->
          forAllValid $ \added -> do
            let size1 = storeSize (store :: ClientStore Int Int Int)
            let store' = addItemToClientStore added store
            let size2 = storeSize store'
            size2 `shouldBe` (size1 + 1)
  describe "addItemToClientStore" $
    it "produces valid stores" $
      producesValid2 (addItemToClientStore @Int @Int @Int)
  describe "emptySyncRequest" $
    it "is valid" $
      shouldBeValid (emptySyncRequest @Int @Int @Int)
  describe "makeSyncRequest" $
    it "produces valid sync requests" $
      producesValid (makeSyncRequest @Int @Int @Int)
  describe "mergeSyncResponse" $ do
    it "produces valid sync stores" $ producesValid2 (mergeSyncResponse @Int @Int @Int)
    it "adds the single item that the server tells it to add to an empty client store" $
      forAllValid $
        \cid ->
          forAllValid $ \a ->
            forAllValid $ \u -> do
              let cstore1 = emptyClientStore {clientStoreAdded = M.singleton (cid :: ClientId) (a :: Int)}
                  resp = emptySyncResponse {syncResponseClientAdded = M.singleton cid (u :: Int)}
                  cstore2 = mergeSyncResponse cstore1 resp
              clientStoreSynced cstore2 `shouldBe` M.singleton u a
  describe "processServerSync" $
    describe "deterministic UUIDs" $
      serverSyncSpec @Int evalDM $
        processServerSync genD

serverSyncSpec ::
  forall a si m.
  (Show si, Ord si, GenValid si, Show a, Ord a, GenValid a, MonadIO m) =>
  (forall r. m r -> IO r) ->
  (ServerStore si a -> SyncRequest ClientId si a -> m (SyncResponse ClientId si a, ServerStore si a)) ->
  Spec
serverSyncSpec eval func = do
  describe "Single client" $ do
    describe "Single-item" $ do
      it "succesfully downloads a single item from the server for an empty client" $
        forAllValid $
          \u ->
            forAllValid $ \i ->
              eval $ do
                let sstore1 = emptyServerStore {serverStoreItems = M.singleton u i}
                let cstore1 = emptyClientStore
                let req = makeSyncRequest cstore1
                (resp, sstore2) <- func sstore1 req
                let cstore2 = mergeSyncResponse cstore1 resp
                liftIO $ do
                  sstore2 `shouldBe` sstore1
                  clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads a single item to the server for an empty server" $
        forAllValid $
          \c ->
            forAllValid $ \i ->
              eval $ do
                let cstore1 = emptyClientStore {clientStoreAdded = M.singleton c i}
                let sstore1 = emptyServerStore
                let req = makeSyncRequest cstore1
                (resp, sstore2) <- func sstore1 req
                let cstore2 = mergeSyncResponse cstore1 resp
                liftIO $ do
                  clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
                  sort (M.elems (clientStoreSynced cstore2))
                    `shouldBe` sort (M.elems $ M.singleton c i)
    describe "Multi-item" $ do
      it "succesfully downloads everything from the server for an empty client" $
        forAllValid $
          \sstore1 ->
            eval $ do
              let cstore1 = emptyClientStore
              let req = makeSyncRequest cstore1
              (resp, sstore2) <- func sstore1 req
              let cstore2 = mergeSyncResponse cstore1 resp
              liftIO $ do
                sstore2 `shouldBe` sstore1
                clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads everything to the server for an empty server" $
        forAllValid $
          \items ->
            eval $ do
              let cstore1 = emptyClientStore {clientStoreAdded = items}
              let sstore1 = emptyServerStore
              let req = makeSyncRequest cstore1
              (resp, sstore2) <- func sstore1 req
              let cstore2 = mergeSyncResponse cstore1 resp
              liftIO $ do
                clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
                sort (M.elems (clientStoreSynced cstore2)) `shouldBe` sort (M.elems items)
      it "is idempotent with one client" $
        forAllValid $
          \cstore1 ->
            forAllValid $ \sstore1 ->
              eval $ do
                let req1 = makeSyncRequest cstore1
                (resp1, sstore2) <- func sstore1 req1
                let cstore2 = mergeSyncResponse cstore1 resp1
                    req2 = makeSyncRequest cstore2
                (resp2, sstore3) <- func sstore2 req2
                let cstore3 = mergeSyncResponse cstore2 resp2
                liftIO $ do
                  cstore2 `shouldBe` cstore3
                  sstore2 `shouldBe` sstore3
  describe "Multiple clients" $ do
    describe "Single-item" $ do
      it "successfully syncs an addition accross to a second client" $
        forAllValid $
          \i ->
            eval $ do
              let cAstore1 = emptyClientStore {clientStoreAdded = M.singleton (ClientId 0) i}
              -- Client B is empty
              let cBstore1 = emptyClientStore
              -- The server is empty
              let sstore1 = emptyServerStore
              -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- func sstore1 req1
              let addedItems = syncResponseClientAdded resp1
              case M.toList addedItems of
                [(ClientId 0, clientAdditionId)] -> do
                  let items = M.singleton clientAdditionId i
                  liftIO $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
                  -- Client A merges the response
                  let cAstore2 = mergeSyncResponse cAstore1 resp1
                  liftIO $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                  -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- func sstore2 req2
                  liftIO $ do
                    resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                    sstore3 `shouldBe` sstore2
                  --  pPrint cBstore2
                  -- Client B merges the response
                  let cBstore2 = mergeSyncResponse cBstore1 resp2
                  liftIO $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                  -- Client A and Client B now have the same store
                  liftIO $ cAstore2 `shouldBe` cBstore2
                _ -> liftIO $ expectationFailure "Should have found exactly one added item."
    describe "Multiple items" $ do
      it
        "makes no change if the sync request reflects the same local state with an empty sync response"
        $ forAllValid
        $ \sis -> do
          let cs = ServerStore sis
          (sr, cs') <-
            eval $
              func cs $
                SyncRequest
                  { syncRequestAdded = M.empty,
                    syncRequestMaximumSynced = fst <$> M.lookupMax sis
                  }
          cs' `shouldBe` cs
          sr
            `shouldBe` SyncResponse
              { syncResponseClientAdded = M.empty,
                syncResponseServerAdded = M.empty
              }
      it "successfully syncs additions accross to a second client" $
        forAllValid $
          \is ->
            eval $ do
              let cAstore1 = emptyClientStore {clientStoreAdded = is}
              -- Client B is empty
              let cBstore1 = emptyClientStore
              -- The server is empty
              let sstore1 = emptyServerStore
              -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- func sstore1 req1
              -- Client A merges the response
              let cAstore2 = mergeSyncResponse cAstore1 resp1
              let items = clientStoreSynced cAstore2
              liftIO $ do
                clientStoreAdded cAstore2 `shouldBe` M.empty
                sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
              liftIO $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
              -- Client B makes sync request 2
              let req2 = makeSyncRequest cBstore1
              -- The server processes sync request 2
              (resp2, sstore3) <- func sstore2 req2
              liftIO $ do
                resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                sstore3 `shouldBe` sstore2
              -- Client B merges the response
              let cBstore2 = mergeSyncResponse cBstore1 resp2
              liftIO $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
              -- Client A and Client B now have the same store
              liftIO $ cAstore2 `shouldBe` cBstore2
  describe "General properties" $ do
    it "produces valid results" $
      forAllValid $
        \cs ->
          forAllValid $ \sr -> do
            res <- eval $ func cs sr
            shouldBeValid res
    it "successfully syncs two clients using a central store" $
      forAllValid $
        \addedItems ->
          eval $ do
            let central = ServerStore M.empty
            let store1 = emptyClientStore {clientStoreAdded = addedItems}
            let store2 = emptyClientStore
            let sreq1 = makeSyncRequest store1
            (sresp1, central') <- func central sreq1
            let store1' = mergeSyncResponse store1 sresp1
            let sreq2 = makeSyncRequest store2
            (sresp2, central'') <- func central' sreq2
            let store2' = mergeSyncResponse store2 sresp2
            let sreq3 = makeSyncRequest store1'
            (sresp3, _) <- func central'' sreq3
            let store1'' = mergeSyncResponse store1' sresp3
            liftIO $ store1'' `shouldBe` store2'
    it "ensures that syncing is idempotent" $
      forAllValid $
        \central1 ->
          forAllValid $ \local1 ->
            eval $ do
              let sreq1 = makeSyncRequest local1
              (sresp1, central2) <- func central1 sreq1
              let local2 = mergeSyncResponse local1 sresp1
              let sreq2 = makeSyncRequest local2
              (sresp2, central3) <- func central2 sreq2
              let local3 = mergeSyncResponse local2 sresp2
              liftIO $ do
                local2 `shouldBe` local3
                central2 `shouldBe` central3

newtype D m a = D
  { unD :: StateT StdGen m a
  }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO)

evalDM :: (Functor m) => D m a -> m a
evalDM d = fst <$> runDM d (mkStdGen 42)

runDM :: D m a -> StdGen -> m (a, StdGen)
runDM = runStateT . unD

genD :: (Monad m) => D m UUID
genD = do
  r <- get
  let (u, r') = random r
  put r'
  pure u
