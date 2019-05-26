open BookType;
let getNothing = () => {
    Js.Promise.(
        Fetch.fetchWithInit(String.concat("/"
                                         ,[ ""
                                         , "nothing"
                                         ])
                           ,Fetch.RequestInit.make(~method_=Get
                                                  ,()))
    |> then_(Fetch.Response.text)
    |> then_(response => (response |> ((x) => if(String.length(x) != 0) { resolve(Belt_Result.Error("Expected the response body to empty")) } else  { resolve(Belt_Result.Ok(x)) }))))
}
