{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Reason.Internal.Generate where

import           Prelude                      hiding ((<$>))
import           Control.Lens                 (to, (^.))
import           Data.Int                     (Int32)
import           Data.List                    (nub)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Encoding           as T
import           Reason                          (ReasonDatatype(..), ReasonPrimitive(..))
import qualified Reason
import           Servant.API                  (NoContent (..))
import           Servant.Reason.Internal.Foreign (LangReason, getEndpoints)
import           Servant.Reason.Internal.Orphans ()
import qualified Servant.Foreign              as F
import           Text.PrettyPrint.Leijen.Text


{-|
Options to configure how code is generated.
-}
data ReasonOptions = ReasonOptions
  { {- | The protocol, host and any path prefix to be used as the base for all
    requests.

    Example: @Static "https://mydomain.com/api/v1"@

    When @Dynamic@, the generated Reason functions take the base URL as the first
    argument.
    -}
    urlPrefix             :: UrlPrefix
  , reasonExportOptions      :: Reason.Options
    -- ^ Options to pass to reason-export
  , emptyResponseReasonTypes :: [ReasonDatatype]
    -- ^ Types that represent an empty Http response.
  , stringReasonTypes        :: [ReasonDatatype]
    -- ^ Types that represent a String.
  , intReasonTypes        :: [ReasonDatatype]
    -- ^ Types that represent a Int.
  , floatReasonTypes        :: [ReasonDatatype]
    -- ^ Types that represent a Float.
  , boolReasonTypes        :: [ReasonDatatype]
    -- ^ Types that represent a Bool.
  , charReasonTypes        :: [ReasonDatatype]
    -- ^ Types that represent a Char.
  }


data UrlPrefix
  = Static T.Text
  | Dynamic


{-|
Default options for generating Reason code.

The default options are:

> { urlPrefix =
>     Static ""
> , reasonExportOptions =
>     Reason.defaultOptions
> , emptyResponseReasonTypes =
>     [ toReasonType NoContent ]
> , stringReasonTypes =
>     [ toReasonType "" ]
> }
-}
defReasonOptions :: ReasonOptions
defReasonOptions = ReasonOptions
  { urlPrefix = Static ""
  , reasonExportOptions = Reason.defaultOptions
  , emptyResponseReasonTypes =
      [ Reason.toReasonType NoContent
      , Reason.toReasonType ()
      ]
  , stringReasonTypes =
      [ Reason.toReasonType ("" :: String)
      , Reason.toReasonType ("" :: T.Text)
      ]
  , intReasonTypes =
      [ Reason.toReasonType (0 :: Int)
      , Reason.toReasonType (0 :: Int32)
      ]
  , floatReasonTypes =
      [ Reason.toReasonType (0 :: Float) ]
  , boolReasonTypes =
      [ Reason.toReasonType (False :: Bool) ]
  , charReasonTypes =
      [ Reason.toReasonType (' ' :: Char) ]
  }


{-|
Default imports required by generated Reason code.

You probably want to include this at the top of your generated Reason module.
-}
defReasonImports :: Text
defReasonImports = T.unlines []


{-|
Generate Reason code for the API with default options.

Returns a list of Reason functions to query your Servant API from Reason.

You could spit these out to a file and call them from your Reason code, but you
would be better off creating a 'Spec' with the result and using 'specsToDir',
which handles the module name for you.
-}
generateReasonForAPI
  :: ( F.HasForeign LangReason ReasonDatatype api
     , F.GenerateList ReasonDatatype (F.Foreign ReasonDatatype api))
  => Proxy api
  -> [Text]
generateReasonForAPI =
  generateReasonForAPIWith defReasonOptions


{-|
Generate Reason code for the API with custom options.
-}
generateReasonForAPIWith
  :: ( F.HasForeign LangReason ReasonDatatype api
     , F.GenerateList ReasonDatatype (F.Foreign ReasonDatatype api))
  => ReasonOptions
  -> Proxy api
  -> [Text]
generateReasonForAPIWith opts =
  nub . map docToText . map (generateReasonForRequest opts) . getEndpoints

i :: Int
i = 4

{-|
Generate an Reason function for one endpoint.
-}
generateReasonForRequest :: ReasonOptions -> F.Req ReasonDatatype -> Doc
generateReasonForRequest opts request =
  funcDef
  where
    funcDef =
      vsep
        [ -- fnName <+> ":" <+> typeSignature
        -- , 
          "let" <+> fnName <+> "=" <+> args <+> "=> {"
        , case letParams of
            Just params ->
              indent i
              (vsep ["let"
                    , indent i params <> ";"
                    , indent i reasonRequest
                    ])
            Nothing ->
              indent i reasonRequest
        , "}"
        ]

    fnName =
      request ^. F.reqFuncName . to (T.replace "-" "" . F.camelCase) . to stext

    typeSignature =
      mkTypeSignature opts request

    args =
      mkArgs opts request

    letParams =
      mkLetParams opts request

    reasonRequest =
      mkRequest opts request


mkTypeSignature :: ReasonOptions -> F.Req ReasonDatatype -> Doc
mkTypeSignature opts request =
  (hsep . punctuate " ->" . concat)
    [ catMaybes [urlPrefixType]
    , headerTypes
    , urlCaptureTypes
    , queryTypes
    , catMaybes [bodyType, returnType]
    ]
  where
    urlPrefixType :: Maybe Doc
    urlPrefixType =
        case (urlPrefix opts) of
          Dynamic -> Just "String"
          Static _ -> Nothing

    reasonTypeRef :: ReasonDatatype -> Doc
    reasonTypeRef eType =
      stext (Reason.toReasonTypeRefWith (reasonExportOptions opts) eType)

    headerTypes :: [Doc]
    headerTypes =
      [ header ^. F.headerArg . F.argType . to reasonTypeRef
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]

    urlCaptureTypes :: [Doc]
    urlCaptureTypes =
        [ F.captureArg capture ^. F.argType . to reasonTypeRef
        | capture <- request ^. F.reqUrl . F.path
        , F.isCapture capture
        ]

    queryTypes :: [Doc]
    queryTypes =
      [ arg ^. F.queryArgName . F.argType . to reasonTypeRef
      | arg <- request ^. F.reqUrl . F.queryStr
      ]

    bodyType :: Maybe Doc
    bodyType =
        fmap reasonTypeRef $ request ^. F.reqBody

    returnType :: Maybe Doc
    returnType = do
      result <- fmap reasonTypeRef $ request ^. F.reqReturnType
      pure ("Http.Request" <+> parens result)


reasonHeaderArg :: F.HeaderArg ReasonDatatype -> Doc
reasonHeaderArg header =
  "header_" <>
  header ^. F.headerArg . F.argName . to (stext . T.replace "-" "_" . F.unPathSegment)


reasonCaptureArg :: F.Segment ReasonDatatype -> Doc
reasonCaptureArg segment =
  "capture_" <>
  F.captureArg segment ^. F.argName . to (stext . F.unPathSegment)


reasonQueryArg :: F.QueryArg ReasonDatatype -> Doc
reasonQueryArg arg =
  "query_" <>
  arg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


reasonBodyArg :: Doc
reasonBodyArg =
  "body"


isNotCookie :: F.HeaderArg f -> Bool
isNotCookie header =
   header
     ^. F.headerArg
      . F.argName
      . to ((/= "cookie") . T.toLower . F.unPathSegment)


mkArgs
  :: ReasonOptions
  -> F.Req ReasonDatatype
  -> Doc
mkArgs opts request =
  (tupled . concat) $
    [ -- Dynamic url prefix
      case urlPrefix opts of
        Dynamic -> ["urlBase"]
        Static _ -> []
    , -- Headers
      [ reasonHeaderArg header
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]
    , -- URL Captures
      [ reasonCaptureArg segment
      | segment <- request ^. F.reqUrl . F.path
      , F.isCapture segment
      ]
    , -- Query params
      [ reasonQueryArg arg
      | arg <- request ^. F.reqUrl . F.queryStr
      ]
    , -- Request body
      maybe [] (const [reasonBodyArg]) (request ^. F.reqBody)
    ]


mkLetParams :: ReasonOptions -> F.Req ReasonDatatype -> Maybe Doc
mkLetParams opts request =
  if null (request ^. F.reqUrl . F.queryStr) then
    Nothing
  else
    Just $ "params =" <$>
           indent i ("List.filter" <> tupled ["((x) => (String.length(x) > 0))", reasonList params])
  where
    params :: [Doc]
    params = map paramToDoc (request ^. F.reqUrl . F.queryStr)

    paramToDoc :: F.QueryArg ReasonDatatype -> Doc
    paramToDoc qarg =
      -- something wrong with indentation here...
      case qarg ^. F.queryArgType of
        F.Normal ->
          let
            toStringSrc' = toStringSrc opts (qarg ^. F.queryArgName . F.argType)
          in
            reasonName
            <$$> indent 2 ("|>" <+> toStringSrc'
                           <$$> "|> Js_global.encodeURIComponent"
                           <$$> "|>" <+> "((x__) => if(String.length(x__) > 0) { "
                                    <> dquotes (reasonName <> equals)
                                    <+> "++ x__ } else {\"\"})")

        F.Flag ->
            "if" <> parens reasonName <+> braces (dquotes (name <> equals)) <+> "else" <+> braces (dquotes empty)

        F.List ->
            "String.concat" <> tupled [dquotes "&"
                                      ,"List.map((x) => "
                                       <> dquotes (name <> "[]=")
                                       <> " ++ Js_global.encodeURIComponent(" <> toStringSrc opts (qarg ^. F.queryArgName . F.argType) <> "(x)), " <> reasonName <> ")"]
      where
        reasonName = reasonQueryArg qarg
        name = qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


mkRequest :: ReasonOptions -> F.Req ReasonDatatype -> Doc
mkRequest opts request =
  "Js.Promise." <> parens (
  line <> indent i
      ("Fetch.fetchWithInit" <>
        tupled [url
               ,"Fetch.RequestInit.make" <>
                tupled (catMaybes [Just $ "~method_=" <> method
                                  ,case headers of
                                      [] -> Nothing
                                      _ -> Just $ "~headers="
                                        <$$> indent 2 "Fetch.HeadersInit.makeWithDict("
                                        <$$> indent 3 ("Js.Dict.fromList(")
                                        <$$> indent 4 (reasonListOfMaybes headers) <> "))"
                                  ,case body of
                                     Nothing -> Nothing
                                     Just b -> Just $ "~body=" <> b
                                  ,Just $ "()"])])
      <$$> "|> then_" <> parens ("Fetch.Response." <> expectResponse)
      <$$> "|> then_" <> parens ("response => (response |>" <+> expectDecoder <> ")"))
       -- , "timeout =" <$>
       --   indent i "Nothing"
       -- ]))
  where
    method =
       request ^. F.reqMethod . to (stext . T.toTitle . T.decodeUtf8)

    mkHeader header =
      let headerName = header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)
          headerArgName = reasonHeaderArg header
          argType = header ^. F.headerArg . F.argType
          wrapped = isReasonMaybeType argType
          toStringSrc' = toStringSrc opts (case argType of
                                             (ReasonPrimitive (ROption t)) -> t
                                             x -> x)
          -- toStringSrc =
          --   if isReasonMaybeStringType opts argType || isReasonStringType opts argType then
          --     mempty
          --   else
          --     " << toString"
      in
        "Belt.Option.map" <> parens ((if wrapped then headerArgName else parens ("Some" <> parens headerArgName))
                                     <> ", x => " <>  parens (dquotes headerName <> ", x |>" <+> toStringSrc'))

    headers =
      (case (request ^. F.reqBody, request ^. F.reqBodyContentType) of
        (Just _, F.ReqBodyJSON) ->
          ["Some" <> parens (parens ("\"Content-Type\"" <> comma <> stext "\"application/json\""))]
        _ -> [])
      ++
      [ mkHeader header
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]

    url =
      mkUrl opts (request ^. F.reqUrl . F.path)
       <> mkQueryParams request

    body =
      case request ^. F.reqBody of
        Nothing -> Nothing
        Just reasonTypeExpr ->
          let encoderName = Reason.toReasonEncoderRefWith (reasonExportOptions opts) reasonTypeExpr
          in Just $ "Fetch.BodyInit.make" <> parens ("Js.Json.stringify" <> parens (stext encoderName <> parens reasonBodyArg))

    expectResponse =
      case request ^. F.reqReturnType of
        Just reasonTypeExpr | isEmptyType opts reasonTypeExpr ->
          "text"
        Just reasonTypeExpr ->
          "json"
        Nothing ->
          error "mkHttpRequest: no reqReturnType?"

    expectDecoder =
      case request ^. F.reqReturnType of
        Just reasonTypeExpr | isEmptyType opts reasonTypeExpr ->
          "((x) => if(String.length(x) != 0) { resolve(Belt_Result.Error(\"Expected the response body to empty\")) } else  { resolve(Belt_Result.Ok(x)) })"
          -- let reasonConstructor =
          --       Reason.toReasonTypeRefWith (reasonExportOptions opts) reasonTypeExpr
          -- in
          --   "Http.expectStringResponse" <$>
          --   indent i (parens (backslash <> braces " body " <+> "->" <$>
          --                     indent i ("if String.isEmpty body then" <$>
          --                               indent i "Ok" <+> stext reasonConstructor <$>
          --                               "else" <$>
          --                               indent i ("Err" <+> dquotes "Expected the response body to be empty")) <> line))
        Just reasonTypeExpr ->
          stext (Reason.toReasonDecoderRefWith (reasonExportOptions opts) reasonTypeExpr) <+> "|> ((x) => Belt_Result.Ok(x)) |> resolve"
        Nothing -> error "mkHttpRequest: no reqReturnType?"


mkUrl :: ReasonOptions -> [F.Segment ReasonDatatype] -> Doc
mkUrl opts segments =
  "String.concat" <> tupled [dquotes "/",
  (reasonList)
    ( case urlPrefix opts of
        Dynamic -> "urlBase"
        Static url -> dquotes (stext url)
      : map segmentToDoc segments)]
  where

    segmentToDoc :: F.Segment ReasonDatatype -> Doc
    segmentToDoc s =
      case F.unSegment s of
        F.Static path ->
          dquotes (stext (F.unPathSegment path))
        F.Cap arg ->
          let
            -- Don't use "toString" on Reason Strings, otherwise we get extraneous quotes.
            toStringSrc' = toStringSrc opts (arg ^. F.argType)
            -- toStringSrc =
            --   if isReasonStringType opts (arg ^. F.argType) then
            --     empty
            --   else
            --     "|> string_of_" <> stext (Reason.toReasonTypeRef (arg ^. F.argType))
          in
            (reasonCaptureArg s) <+> "|>" <+> toStringSrc' <+> "|>" <+> "Js_global.encodeURIComponent"


mkQueryParams
  :: F.Req ReasonDatatype
  -> Doc
mkQueryParams request =
  if null (request ^. F.reqUrl . F.queryStr) then
    empty
  else
    line <> "++" <+> align ("if(List.length(params)==0)" <+> braces (dquotes empty)
                            <+> "else" <+> braces (dquotes "?" <+> "++ String.concat" <> tupled [dquotes "&", "params"]))


{- | Determines whether we construct an Reason function that expects an empty
response body.
-}
isEmptyType :: ReasonOptions -> ReasonDatatype -> Bool
isEmptyType opts reasonTypeExpr =
  reasonTypeExpr `elem` emptyResponseReasonTypes opts

{- | Determines how to stringify a value.
-}
toStringSrc :: ReasonOptions -> ReasonDatatype -> Doc
toStringSrc opts argType
  -- Don't use "toString" on Reason Strings, otherwise we get extraneous quotes.
  -- We don't append an operator in this case
  | isReasonStringType opts argType = stext "((x) => x)"
  | otherwise                    = toStringSrcTypes opts argType


toStringSrcTypes :: ReasonOptions -> ReasonDatatype -> Doc
toStringSrcTypes opts (ReasonPrimitive (ROption argType)) =
  "((x) => Belt.Option.mapWithDefault(x, \"\", " <> toStringSrcTypes opts argType <> "))" 
  -- "Maybe.map (" <> toStringSrcTypes operator opts argType <> ") |> Maybe.withDefault \"\""
 -- [Char] == String so we can just use identity here.
 -- We can't return `""` here, because this string might be nested in a `Maybe` or `List`.
toStringSrcTypes _ (ReasonPrimitive (RList (ReasonPrimitive RChar))) = "((x) => x)"
toStringSrcTypes opts (ReasonPrimitive (RList argType)) = toStringSrcTypes opts argType
toStringSrcTypes opts argType
    | isReasonStringType opts argType = "((x) => x)"
    | isReasonIntType opts argType    = "string_of_int"
    | isReasonFloatType opts argType  = "string_of_float"
    | isReasonBoolType opts argType   = "string_of_bool" -- We should change this to return `true`/`false` but this mimics the old behavior.
    | isReasonCharType opts argType   = "string_of_char"
    | otherwise                         = error $ "Sorry, we don't support other types than `String`, `Int`, `Float`, `Bool`, and `Char` right now. " <> show argType



{- | Determines whether we call `toString` on URL captures and query params of
this type in Reason.
-}
isReasonStringType :: ReasonOptions -> ReasonDatatype -> Bool
isReasonStringType opts reasonTypeExpr =
  reasonTypeExpr `elem` stringReasonTypes opts

{- | Determines whether we call `String.fromInt` on URL captures and query params of this type in Reason.
-}
isReasonIntType :: ReasonOptions -> ReasonDatatype -> Bool
isReasonIntType opts reasonTypeExpr =
  reasonTypeExpr `elem` intReasonTypes opts


{- | Determines whether we call `String.fromFloat` on URL captures and query params of
this type in Reason.
-}
isReasonFloatType :: ReasonOptions -> ReasonDatatype -> Bool
isReasonFloatType opts reasonTypeExpr =
  reasonTypeExpr `elem` floatReasonTypes opts


{- | Determines whether we convert to `true` or `false`
-}
isReasonBoolType :: ReasonOptions -> ReasonDatatype -> Bool
isReasonBoolType opts reasonTypeExpr =
  reasonTypeExpr `elem` boolReasonTypes opts

{- | Determines whether we call `String.fromChar` on URL captures and query params of
this type in Reason.
-}
isReasonCharType :: ReasonOptions -> ReasonDatatype -> Bool
isReasonCharType opts reasonTypeExpr =
  reasonTypeExpr `elem` charReasonTypes opts

{- | Determines whether a type is 'Maybe a' where 'a' is something akin to a 'String'.
-}
isReasonMaybeStringType :: ReasonOptions -> ReasonDatatype -> Bool
isReasonMaybeStringType opts (ReasonPrimitive (ROption reasonTypeExpr)) = reasonTypeExpr `elem` stringReasonTypes opts
isReasonMaybeStringType _ _ = False

isReasonMaybeType :: ReasonDatatype -> Bool
isReasonMaybeType (ReasonPrimitive (ROption _)) = True
isReasonMaybeType _ = False


-- Doc helpers


docToText :: Doc -> Text
docToText =
  L.toStrict . displayT . renderPretty 0.4 100

stext :: Text -> Doc
stext = text . L.fromStrict

reasonRecord :: [Doc] -> Doc
reasonRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

reasonList :: [Doc] -> Doc
reasonList [] = lbracket <> rbracket
reasonList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket

reasonListOfMaybes :: [Doc] -> Doc
reasonListOfMaybes [] = lbracket <> rbracket
reasonListOfMaybes ds = "Belt.List.keepMap" <> parens (line <> reasonList ds <> ", x => x")
