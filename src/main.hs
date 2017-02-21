
import Azubi.Model
import Azubi.Boot
import Azubi.StateExecuter
import Azubi.Syntax


main :: IO ()
main = defaultMain $ []
  & content "/dev/shm/azubi.test" [ "Das ist ein"
                                  , "Azubi Test"
                                  ]







