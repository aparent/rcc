import Options.Applicative
import ParseJanus
import GenJanus
import Circuit
import CircuitDiagram
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
        print $ circ
        putStrLn $ writeQC circ
        putStrLn $ formatSimOutput $ simulate circ
        circuitToSvg circ "circ.svg" 1000
  return ()

formatSimOutput :: [(String,Integer)] -> String
formatSimOutput vals = "\nSimulation Results:" ++ vs
  where vs = concatMap (\(var,val) -> '\n' : '\t' : var ++ " = " ++ show val) vals
