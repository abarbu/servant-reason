type book = { title : string}

let rec decodeBook = json =>
  {title : json |> Json.Decode.field ("title" ,Json.Decode.string),}

let rec encodeBook  = (x : book) => 
  Json.Encode.object_ ([ ( "title", Json.Encode.string(x.title) )])
