import Options.Applicative
import ParseJanus
import GenJanus
import Circuit

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
             <> value 8)

main = do
  options <- execParser opts
  progStr <- readFile (inputFile options)
  let pj = parseJanus (inputFile options) progStr
  case pj of
    Left pe -> print pe
    Right jan -> putStrLn $ writeQC $ genJanus (intSize options) jan
  return ()
