module Program.Tar where

import Prologue

import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Program                    as Program
import qualified Progress                   as Progress
import qualified Utils                      as Utils

import Conduit              (linesUnboundedAsciiC, sinkNull, (.|), ConduitM 
                            , sinkList)
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.ByteString      (ByteString)
import Program              (Program)
import System.Directory     (createDirectoryIfMissing)
import System.Exit          (ExitCode (ExitFailure, ExitSuccess))



--------------------------
-- === Tar program  === --
--------------------------

data Tar
instance Program Tar where
    executableName = "tar"


----------------------
-- === Command  === --
----------------------
    
data Format = GZIP | BZIP2 | XZ | LZMA deriving (Show, Eq)

instance Program.Argument Format where
    format = \case
        GZIP  -> ["-z"]
        BZIP2 -> ["-j"]
        XZ    -> ["-J"]
        LZMA  -> ["--lzma"]

data Command = Create | Append | List | Extract deriving (Show, Eq)
instance Program.Argument Command where
    format = \case
        Create  -> ["-c"]
        Append  -> ["-r"]
        List    -> ["-t"]
        Extract -> ["-x"]

data Switch 
    = TargetFile FilePath
    | Verbose
    | UseFormat Format
    | ChangeDirectory FilePath
instance Program.Argument Switch where
    format = \case
        TargetFile path      -> ["-f", path]
        Verbose              -> ["-v"]
        UseFormat f          -> Program.format f
        ChangeDirectory path -> ["-C", path]

callArgs :: Command -> [Switch] -> FilePath -> [FilePath] -> [String]
callArgs cmd switches archivePath targets = 
           Program.format cmd
        <> (Program.format $ TargetFile archivePath)
        <> Program.format switches
        <> targets

call :: MonadIO m => Command -> [Switch] -> FilePath -> [FilePath] -> m ()
call = Program.call @Tar .:: callArgs



-----------------------
-- === Progress  === --
-----------------------

data UnpackProgressInfo = UnpackProgressInfo
    { _doneFiles :: Int
    , _allFiles  :: Int
    } deriving (Show)
makeLenses ''UnpackProgressInfo

updateChunk 
    :: MonadIO m 
    => (UnpackProgressInfo -> IO ()) -- ^ Callback.
    -> Int -- ^ Total file count.
    -> ConduitM ByteString ByteString m ()
updateChunk callback totalCount = 
    Utils.processChunk 1 $ \currentCount _ -> do
        liftIO $ callback $ UnpackProgressInfo currentCount totalCount
        pure $ currentCount + 1

instance Progress.Progress UnpackProgressInfo where
    ratio progressInfo = 
        Just $ fromIntegral (progressInfo ^. doneFiles) 
             / fromIntegral (progressInfo ^. allFiles)



------------------
-- === API  === --
-------------------

pack :: (MonadIO m) => [FilePath] -> FilePath -> Format -> m ()
pack pathsToPack archivePath format = 
    call Create [UseFormat format] archivePath pathsToPack

-- | Returns the number of file in the archive
fileCount :: (MonadIO m) => FilePath -> m Int
fileCount path = do
    (result, _) <- Program.read' @Tar $ callArgs List [] path []
    pure $ length $ BS8.lines result

-- | Unpack all files from given archive into directory.
unpack 
    :: (MonadIO m) 
    => FilePath -- ^ Output directory
    -> FilePath -- ^ Archive
    -> m ()
unpack outputDirectory archivePath = do
    liftIO $ createDirectoryIfMissing True outputDirectory
    call Extract [ChangeDirectory outputDirectory] archivePath []

-- | Unpack all files from given archive into directory. Note: progress tracking
--   relies on number of lines printed, may not be reliable, particularly when
--   error are encountered.
unpackWithProgress
    :: (MonadIO m, MonadMask m)
    => Progress.Observer UnpackProgressInfo -- ^ callback
    -> FilePath -- ^ Output directory
    -> FilePath -- ^ Archive
    -> m ()
unpackWithProgress callback = 
    Progress.runProgressible callback .: unpackWithProgress'

-- | Unpack all files from given archive into directory. Note: progress tracking
--   relies on number of lines printed, may not be reliable, particularly when
--   error are encountered.
unpackWithProgress'
    :: (MonadIO m) 
    => FilePath -- ^ Output directory
    -> FilePath -- ^ Archive
    -> (UnpackProgressInfo -> IO ()) -- ^ callback
    -> m ()
unpackWithProgress' outputDirectory archivePath callback = do
    let switches = [ChangeDirectory outputDirectory, Verbose]
    count <- fileCount archivePath
    procCfg <- Program.prog' @Tar $ callArgs Extract switches archivePath []
    liftIO $ createDirectoryIfMissing True outputDirectory
    let inputConduit = pure ()
    let outConduit = sinkNull
    let errConduit = linesUnboundedAsciiC 
                  .| updateChunk callback count 
                  .| sinkList
    
    (result, _, err) <- liftIO
        $ sourceProcessWithStreams procCfg inputConduit outConduit errConduit
    case result of
        ExitSuccess      -> pure ()
        -- TODO report properly error, including "err" in payload (it is where
        -- actual error message will be placed)
        ExitFailure _ -> do
            print err
            error $ "failed to unpack " <> archivePath 
                 <> " to " <> outputDirectory
