open BookType;
let getBooksByTitle = (capture_title) => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ ""
                                         , "books"
                                         , capture_title |> ((x) => x) |> Js_global.encodeURIComponent
                                         ])
                           ,Fetch.RequestInit.make(~method_=Get
                                                  ,()))
    |> then_(Fetch.Response.json)
    |> then_(response => (response |> decodeBook |> ((x) => Belt_Result.Ok(x)) |> resolve)))
}
