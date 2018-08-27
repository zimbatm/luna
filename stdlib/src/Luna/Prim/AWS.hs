{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.AWS where

import Prologue

import Control.Lens     (to)
import Data.ByteString  (ByteString)
import Data.Map         (Map)
import Data.Text        (Text)
import Luna.Std.Builder (LTp (..), integer, makeFunctionIO, makeFunctionPure)
import System.FilePath  ((<.>), (</>))

import qualified Codec.Archive.Zip           as Zip
import qualified Data.ByteString             as BS
import qualified Data.Map                    as Map
import qualified Data.Text.IO                as Text
import qualified Luna.IR                     as IR
import qualified Luna.Pass.Sourcing.Data.Def as Def
import qualified Luna.Runtime                as Luna
import qualified Luna.Std.Builder            as Builder
import qualified Network.AWS                 as AWS
import qualified Network.AWS.Lambda          as Lambda
import qualified Network.AWS.Lambda.Types    as Lambda
import qualified OCI.Data.Name               as Name
import qualified System.Directory            as Dir

type AWSModule = "Std.AWS"

awsModule :: Name.Qualified
awsModule = Name.qualFromSymbol @AWSModule

data FunctionInfo = FunctionInfo
    { _name :: Text
    , _env  :: AWS.Env
    , _conf :: Lambda.FunctionConfiguration
    }
makeLenses ''FunctionInfo


exports :: forall graph m. Builder.StdBuilder graph m => m (Map IR.Name Def.Def)
exports = do
    let envT   = LCons awsModule "AWSEnv" []
        fConfT = LCons awsModule "LambdaFunctionConfiguration" []

    let funcNameFromConfVal :: Lambda.FunctionConfiguration -> Text
        funcNameFromConfVal = fromJust "" . view Lambda.fcFunctionName
    primFuncNameFromConf <- makeFunctionPure @graph
        (flip Luna.toValue funcNameFromConfVal)
        [fConfT] Builder.textLT

    let listFunsVal :: AWS.Env -> IO [Lambda.FunctionConfiguration]
        listFunsVal env = do
            res <- AWS.runResourceT . AWS.runAWS env . AWS.send $ Lambda.listFunctions
            pure $ res ^. Lambda.lfrsFunctions
        returnT = Builder.listLT fConfT
    primListFuns <- makeFunctionIO @graph
        (flip Luna.toValue listFunsVal)[envT]
        returnT

    let newEnvVal :: IO AWS.Env
        newEnvVal = AWS.newEnv AWS.Discover
    primAWSNewEnv <- makeFunctionIO @graph (flip Luna.toValue newEnvVal) [] envT

    let invokeVal :: AWS.Env -> Text -> ByteString -> IO Lambda.InvokeResponse
        invokeVal env fname payload =
            let invocation = Lambda.invoke fname payload
            in AWS.runResourceT . AWS.runAWS env . AWS.send $ invocation
        invokeArgsT = [envT, Builder.textLT, Builder.binaryLT]
        invokeRespT = LCons awsModule "LambdaInvokeResponse" []
    primAWSInvoke <- makeFunctionIO @graph
        (flip Luna.toValue invokeVal)
        invokeArgsT invokeRespT

    let zipFunctionCodeVal :: Text -> Text -> IO ByteString
        zipFunctionCodeVal fname contents = do
            let contentsBS = convertTo @ByteString contents
                dirName    = convertTo @String     fname
                archName   = dirName <.> "zip"
                fileName   = "index" <.> "js"
            s <- Zip.mkEntrySelector fileName -- (dirName </> fileName)
            let newEntry = Zip.addEntry Zip.Store contentsBS s
            Zip.createArchive archName newEntry
            bs <- BS.readFile archName
            Dir.removeFile archName
            pure bs
    primZipFunctionCode <- makeFunctionIO @graph
        (flip Luna.toValue zipFunctionCodeVal)
        [Builder.textLT, Builder.textLT]
        Builder.binaryLT

    let createFunctionVal :: AWS.Env -> Text -> Text -> ByteString -> IO FunctionInfo
        createFunctionVal env fname role code = do
            let fcode   = Lambda.functionCode & Lambda.fcZipFile .~ (Just code)
                rtime   = Lambda.NODEJS8_10
                handler = "index.handler"
                createF = Lambda.createFunction fname rtime role handler fcode

            conf <- AWS.runResourceT . AWS.runAWS env . AWS.send $ createF
            pure $ FunctionInfo fname env conf
        functionInfoT = LCons awsModule "LambdaFunction" []
    primAWSCreateFunction <- makeFunctionIO @graph
        (flip Luna.toValue createFunctionVal)
        [envT, Builder.textLT, Builder.textLT, Builder.binaryLT]
        functionInfoT

    pure $ Map.fromList [ ("primAWSListFuns",         primListFuns)
                        , ("primAWSFuncNameFromConf", primFuncNameFromConf)
                        , ("primAWSNewEnv",           primAWSNewEnv)
                        , ("primAWSInvoke",           primAWSInvoke)
                        , ("primZipFunctionCode",     primZipFunctionCode)
                        , ("primAWSCreateFunction",   primAWSCreateFunction)
                        ]


-- === AWS.Env === --
type instance Luna.RuntimeRepOf AWS.Env =
    Luna.AsNative ('Luna.ClassRep AWSModule "AWSEnv")

-- === Lambda FunctionConfiguration === --
type instance Luna.RuntimeRepOf Lambda.FunctionConfiguration =
    Luna.AsNative ('Luna.ClassRep AWSModule "LambdaFunctionConfiguration")

-- === Lambda.InvokeResponse === --
type instance Luna.RuntimeRepOf Lambda.InvokeResponse =
    Luna.AsClass Lambda.InvokeResponse ('Luna.ClassRep "Std.AWS" "LambdaInvokeResponse")

instance Luna.ToObject Lambda.InvokeResponse where
    toConstructor imps v = Luna.Constructor "LambdaInvokeResponse"
        [ Luna.toData imps $ v ^. Lambda.irsStatusCode . to integer
        , Luna.toData imps $ v ^. Lambda.irsPayload
        ]

-- === FunctionInfo === --
type instance Luna.RuntimeRepOf FunctionInfo =
    Luna.AsClass FunctionInfo ('Luna.ClassRep "Std.AWS" "LambdaFunction")

instance Luna.ToObject FunctionInfo where
    toConstructor imps (FunctionInfo name env conf) = Luna.Constructor "LambdaFunction"
        [ Luna.toData imps name
        , Luna.toData imps env
        , Luna.toData imps conf
        ]
