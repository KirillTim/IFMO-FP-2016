import Control.Monad.Writer (Writer, execWriter, tell)


listTellExample :: Writer [Int] ()
listTellExample = tell [1] >> tell [2] >> tell [3]
