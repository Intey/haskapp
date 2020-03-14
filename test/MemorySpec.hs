module MemorySpec where
import Storage.Memory2
import Domain.Model
import qualified UseCases.Client as UCC
import Test.Hspec
import UseCases.Client

spec = hspec $ do
    describe "memory storage" $ do
        it "should create object" $
            (objectGetAll . snd $ objectSave (emptyObjectsStore :: MemoryObjectsStorage) (Doc "example")) `shouldBe` [Doc "example"]
        
        it "should save in fullfilled store with correct id" $
            fst (objectSave (store [(1, (Doc "example"))]) (Doc "example") ) `shouldBe` 2
        
        it "should give access to objects by id" $
            objectGetById (store [(1, (Doc "example"))]) 1 `shouldBe` (Just (Doc "example"))
        
        it "should return Nothing, when no data exists" $
            objectGetById (emptyObjectsStore :: MemoryObjectsStorage) 1 `shouldBe` Nothing
        
        it "should return Nothing, when no matching id found" $
            objectGetById (store ([(1, (Doc "example"))])) 2 `shouldBe` Nothing
    -- describe "Use case with storage" $ do
    --     it "should has nice interface" $
    --         let store = (MemoryStorage []), view = ConsoleView 
    --             in
    --                 (UCC.viewOjects store view) ) `shouldBe` Nothing