import Options.Applicative
import ParseJanus
import GenJanus
import Circuit

data Options = Options
  { inputFile :: String}

opts :: ParserInfo Options
opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Compile TARGET"
     <> header "jcc - A circuit compiler for Janus" )
  where options =
          Options
          <$> argument str (metavar "TARGET")

main = do
  options <- execParser opts
  progStr <- readFile (inputFile options)
  let pj = parseJanus (inputFile options) progStr
  case pj of
    Left pe -> print pe
    Right jan -> putStrLn $ writeQC $ genJanus 8 jan
  return ()
