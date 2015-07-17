import Prelude
import Options.Applicative
import ParseJanus
import GenJanus
import Circuit
import CircuitDiagram
import CircuitOptimize
import Simulate

data Options = Options
  { inputFile :: String,
    intSize :: Int }

opts :: ParserInfo Options
opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Compile TARGET"
     <> header "jcc - A circuit compiler for Janus" )
  where options =
          Options
          <$> argument str (metavar "TARGET")
          <*> option auto
              ( long "int-size"
             <> short 's'
             <> metavar "SIZE"
             <> help "Specify how many bit should be used to represent an integer"
             <> value 4)

main :: IO ()
main = do
  options <- execParser opts
  progStr <- readFile (inputFile options)
  let pj = parseJanus (inputFile options) progStr
  case pj of
    Left pe -> print pe
    Right jan -> do
        let circ = genJanus (intSize options) jan
        let circOpt = optimize circ
        print circ
        putStrLn $ writeQC circOpt
        putStrLn $ formatSimOutput $ simulate circOpt
        circuitToSvg circ "circ.svg" 1000
        circuitToSvg circOpt "circOpt.svg" 1000
  return ()

formatSimOutput :: ([(String,Integer)] , Integer) -> String
formatSimOutput (vals,anc) = "\nSimulation Results:"
                          ++ vs
                          ++ "\nAncilla (zero means cleared): "
                          ++ show anc
  where vs = concatMap (\(var,val) -> '\n' : '\t' : var ++ " = " ++ show val) vals
