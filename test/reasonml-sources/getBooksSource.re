open BookType;
let getBooks = (query_published
               ,query_sort
               ,query_year
               ,query_category
               ,query_filters) => {
    let
        params =
            List.filter(((x) => (String.length(x) > 0))
                       ,[ if(query_published) {"published="} else {""}
                       , query_sort
                         |> ((x) => Belt.Option.mapWithDefault(x, "", ((x) => x)))
                         |> Js_global.encodeURIComponent
                         |> ((x__) => if(String.length(x__) > 0) { "query_sort=" ++ x__ } else {""})
                       , query_year
                         |> ((x) => Belt.Option.mapWithDefault(x, "", string_of_int))
                         |> Js_global.encodeURIComponent
                         |> ((x__) => if(String.length(x__) > 0) { "query_year=" ++ x__ } else {""})
                       , query_category
                         |> ((x) => x)
                         |> Js_global.encodeURIComponent
                         |> ((x__) => if(String.length(x__) > 0) { "query_category=" ++ x__ } else {""})
                       , String.concat("&"
                                      ,List.map((x) => "filters[]=" ++ Js_global.encodeURIComponent(((x) => Belt.Option.mapWithDefault(x, "", string_of_bool))(x)), query_filters))
                       ]);
        Js.Promise.(
            Fetch.fetchWithInit(String.concat("/"
                                             ,[ ""
                                             , "books"
                                             ])
                               ++ if(List.length(params)==0) {""} else {"?" ++ String.concat("&"
                                                                                            ,params)}
                               ,Fetch.RequestInit.make(~method_=Get
                                                      ,()))
        |> then_(Fetch.Response.json)
        |> then_(response => (response |> Json.Decode.list(decodeBook) |> ((x) => Belt_Result.Ok(x)) |> resolve)))
}
