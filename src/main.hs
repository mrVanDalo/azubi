
import Azubi.Model
import Azubi.Boot
import Azubi.StateExecutor
import Azubi.Syntax


main :: IO ()
main = defaultMain $ []
  & content "/dev/shm/azubi.test" [ "Das ist ein"
                                  , "Azubi Test"
                                  ]







