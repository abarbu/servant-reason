open BookType;
let getWitharesponseheader = () => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ ""
                                         , "with-a-response-header"
                                         ])
                           ,Fetch.RequestInit.make(~method_=Get
                                                  ,()))
    |> then_(Fetch.Response.json)
    |> then_(response => (response |> Json.Decode.string |> ((x) => Belt_Result.Ok(x)) |> resolve)))
}
