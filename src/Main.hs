import Options.Applicative
import ParseJanus

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
  print $ parseJanus (inputFile options) progStr
  return ()
