module Infrastructure where
    
data UseCaseView = ConsoleView 
                 | FileView
    deriving (Show, Eq)