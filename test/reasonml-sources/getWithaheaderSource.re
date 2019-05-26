open BookType;
let getWithaheader = (header_myStringHeader
                     ,header_MyIntHeader
                     ,header_MyRequiredStringHeader
                     ,header_MyRequiredIntHeader) => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ ""
                                         , "with-a-header"
                                         ])
                           ,Fetch.RequestInit.make(~method_=Get
                                                  ,~headers=
                                                    Fetch.HeadersInit.makeWithDict(
                                                     Js.Dict.fromList(
                                                      Belt.List.keepMap(
                                                      [ Belt.Option.map(header_myStringHeader, x => ("myStringHeader", x |> ((x) => x)))
                                                      , Belt.Option.map(header_MyIntHeader, x => ("MyIntHeader", x |> string_of_int))
                                                      , Belt.Option.map((Some(header_MyRequiredStringHeader)), x => ("MyRequiredStringHeader", x |> ((x) => x)))
                                                      , Belt.Option.map((Some(header_MyRequiredIntHeader)), x => ("MyRequiredIntHeader", x |> string_of_int))
                                                      ], x => x)))
                                                  ,()))
    |> then_(Fetch.Response.json)
    |> then_(response => (response |> Json.Decode.string |> ((x) => Belt_Result.Ok(x)) |> resolve)))
}
