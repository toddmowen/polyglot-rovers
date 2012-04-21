#!/usr/bin/env runghc -i..

import Monadic.Rovers
import Monadic.Parser


main = interact (unlines . map show . runScenario . parseScenario "stdin")

runScenario s = [runRover r (sequence_ cmds) | (r,cmds) <- getRovers s]
