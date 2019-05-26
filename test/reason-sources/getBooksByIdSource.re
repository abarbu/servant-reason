open BookType;
let getBooksById = (capture_id) => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ ""
                                         , "books"
                                         , capture_id |> string_of_int |> Js_global.encodeURIComponent
                                         ])
                           ,Fetch.RequestInit.make(~method_=Get
                                                  ,()))
    |> then_(Fetch.Response.json)
    |> then_(response => (response |> decodeBook |> ((x) => Belt_Result.Ok(x)) |> resolve)))
}
