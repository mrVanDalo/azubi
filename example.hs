#!/usr/bin/env runghc


import Azubi


main :: IO()
main = defaultMain $ []
  & content "/dev/shm/azubi.test" ["this ist a test"]
  & link "/dev/shm/azubi.link" "/dev/shm/azubi.test"
