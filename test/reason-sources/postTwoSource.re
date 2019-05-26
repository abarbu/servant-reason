let postTwo = (body) => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ ""
                                         , "two"
                                         ])
                           ,Fetch.RequestInit.make(~method_=Post
                                                  ,~headers=
                                                    Fetch.HeadersInit.makeWithDict(
                                                     Js.Dict.fromList(
                                                      Belt.List.keepMap(
                                                      [ Some(("Content-Type","application/json"))
                                                      ], x => x)))
                                                  ,~body=Fetch.BodyInit.make(Js.Json.stringify(Json.Encode.string(body)))
                                                  ,()))
    |> then_(Fetch.Response.json)
    |> then_(response => (response |> Json.Decode.optional(Json.Decode.int) |> ((x) => Belt_Result.Ok(x)) |> resolve)))
}
