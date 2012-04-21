#!/usr/bin/env runghc -i..

import Monadic.Rovers
import Monadic.Parser


main = interact (unlines . map show . getRovers . parseScenario "stdin")
