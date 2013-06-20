module System.Saga.Cmd where
import Shelly
import Prelude hiding (FilePath)
import Data.Text.Lazy as LT
default (LT.Text)

-- | Actual Program to do the work
progName :: FilePath
progName = "/usr/bin" </> "saga_cmd"

-- | Wrapper around saga
saga ::
      Text                  -- ^ Library name
    -> Text                  -- ^ Module name  -- 
    -> [(Text,Text)]         -- ^ Parameter key-value (except of input)
    -> String                -- ^ Parameter input-key
    -> FilePath                -- ^ Input-file
    -> ShIO FilePath         -- ^ Output-file
saga lib mod params fKey inF = do
    result <- run progName [lib, mod]
    return ("." </> outF )
  where
    renderParams = Prelude.map renderPara
    renderPara (k,v) = "-" ++ k ++ " " ++ v
    outF = replace (toTextIgnore inF) (pack " ") (pack "_")

-- | Read an XyzGrid 
readXyzGrid ::
      Double                    -- ^ Cellsize of grid
    -> Text                      -- ^ Seperator
    -> FilePath                  -- ^ Input File
    -> ShIO FilePath
readXyzGrid cs sep f = saga ""
