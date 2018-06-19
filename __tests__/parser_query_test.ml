open Jest
open Expect

let () = describe "Query" (fun () -> 
  let dict = 
    Dict.empty
    |> Dict.add "str_key" ["value"]
    |> Dict.add "int_key" ["55"]
    |> Dict.add "person" ["john"]
  in

  describe "#string" (fun () -> 
    test "returns some string when key exists" (fun () -> 
      expect (Parser_query.parse (Parser_query.string "str_key") dict) |> toEqual( Some "value" )
    );
    test "return nothing when key doesn't exist" (fun () ->
      expect (Parser_query.parse (Parser_query.string "does-not-exist") dict) |> toEqual( None )
    );
  );

  describe "#int" (fun () -> 
    test "returns optional int when key exists" (fun () -> 
      expect (Parser_query.parse (Parser_query.int "int_key") dict) |> toEqual( Some 55 )
    );
    test "returns nothing when key doesn't exists" (fun () ->
      expect (Parser_query.parse (Parser_query.int "does-not-exist") dict) |> toEqual( None )
    );
  );
  
  describe "#enum" (fun () -> 
    let favorite_colors = Dict.add "john" "red" Dict.empty in 
    test "returns optional int when key exists" (fun () -> 
      expect (Parser_query.parse (Parser_query.enum "person" favorite_colors) dict) |> toEqual( Some "red" )
    );
  );

  describe "#map" (fun () -> 
    test "returns optional int when key exists" (fun () -> 
      let parser = 
        Parser_query.string "int_key"
        |> Parser_query.map (fun x -> Belt.Option.getWithDefault x "0")
        |> Parser_query.map  
          (fun x -> 
            try
              Some (int_of_string x)
            with
            | _ -> None
          ) 
      in
      expect (Parser_query.parse parser dict) |> toEqual( Some 55 )
    );
  );
)