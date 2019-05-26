open BookType;
let postBooks = (body) => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ ""
                                         , "books"
                                         ])
                           ,Fetch.RequestInit.make(~method_=Post
                                                  ,~headers=
                                                    Fetch.HeadersInit.makeWithDict(
                                                     Js.Dict.fromList(
                                                      Belt.List.keepMap(
                                                      [ Some(("Content-Type","application/json"))
                                                      ], x => x)))
                                                  ,~body=Fetch.BodyInit.make(Js.Json.stringify(encodeBook(body)))
                                                  ,()))
    |> then_(Fetch.Response.text)
    |> then_(response => (response |> ((x) => if(String.length(x) != 0) { resolve(Belt_Result.Error("Expected the response body to empty")) } else  { resolve(Belt_Result.Ok(x)) }))))
}
