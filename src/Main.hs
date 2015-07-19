import Prelude
import Options.Applicative
import ParseJanus
import GenJanus
import Circuit
import CircuitDiagram
import CircuitOptimize
import Simulate

data Options = Options
  { inputFile  :: String,
    intSize    :: Int,
    diagram    :: Maybe String,
    optimizeOn :: Bool }

main :: IO ()
main = do
  options <- execParser opts
  progStr <- readFile (inputFile options)
  let pj = parseJanus (inputFile options) progStr
  case pj of
    Left pe -> print pe
    Right jan -> do
        let circ = mkCirc jan options
        print circ
        putStrLn . writeQC $ circ
        putStrLn . formatSimOutput . simulate $ circ
        case diagram options of
          Nothing -> return ()
          Just fn ->
            circuitToSvg circ fn 1000
  return ()

mkCirc :: Janus -> Options -> Circuit
mkCirc jan options
  | optimizeOn options = optimize . genJanus (intSize options) $ jan
  | otherwise = genJanus (intSize options) jan

formatSimOutput :: ([(String,Integer)] , Integer) -> String
formatSimOutput (vals,anc) = "\nSimulation Results:"
                          ++ vs
                          ++ "\nAncilla (zero means cleared): "
                          ++ show anc
  where vs = concatMap (\(var,val) -> '\n' : '\t' : var ++ " = " ++ show val) vals


opts :: ParserInfo Options
opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Compile TARGET"
     <> header "jcc - A circuit compiler for Janus" )
  where defIntSize = 4
        options =
          Options
          <$> argument str (metavar "TARGET")
          <*> option auto
              ( short 's'
             <> metavar "SIZE"
             <> help ("Specify how many bit should be used to represent an integer (default "
                  ++ show defIntSize ++" bits)")
             <> value defIntSize)
          <*> optional (strOption $
                short 'd'
             <> metavar "FILENAME"
             <> help "Write an svg to the specified file" )
          <*> switch
              ( short 'O'
              <> help "Turn on optimizations" )
