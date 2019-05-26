open BookType;
let getOne = (urlBase) => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ urlBase
                                         , "one"
                                         ])
                           ,Fetch.RequestInit.make(~method_=Get
                                                  ,()))
    |> then_(Fetch.Response.json)
    |> then_(response => (response |> Json.Decode.int |> ((x) => Belt_Result.Ok(x)) |> resolve)))
}
