open Jest
open Expect

let () = describe "Builder" (fun () -> 

  describe "#absolute" (fun () -> 
    test "returns absolute path" (fun () -> 
      expect (Builder.absolute ["my"; "path"] [("k1", "v1"); ("k2", "v2") ])
      |> toEqual("/my/path?k1=v1&k2=v2")
    );
  );

  describe "#relative" (fun () -> 
    test "returns relative path" (fun () -> 
      expect (Builder.relative ["my"; "path"] [("k1", "v1"); ("k2", "v2") ])
      |> toEqual("my/path?k1=v1&k2=v2")
    );
  );

  describe "#cross_origin" (fun () -> 
    test "returns path with host" (fun () -> 
      expect (Builder.cross_origin "http://www.example.com" ["my"; "path"] [("k1", "v1"); ("k2", "v2") ]) 
      |> toEqual("http://www.example.com/my/path?k1=v1&k2=v2")
    );
  );

  describe "#custom" (fun () -> 
    test "returns url string" (fun () -> 
      expect 
        (Builder.custom 
          (Builder.CrossOrigin "http://www.example.com") 
          ["my"; "path"] 
          [("k1", "v1"); ("k2", "v2")] 
          (Some "frag") 
        ) 
      |> toEqual("http://www.example.com/my/path?k1=v1&k2=v2#frag")
    );
  );
  
  describe "#string" (fun () -> 
    test "generates percent encoded string query parameter" (fun () -> 
      expect (Builder.string "my key" "my value")
      |> toEqual ("my%20key", "my%20value")
    );
  );

  describe "#int" (fun () -> 
    test "generates percent encoded int query parameter" (fun () -> 
      expect (Builder.int "my key" 123)
      |> toEqual ("my%20key", "123")
    );
  );

  describe "#to_query" (fun () -> 
    test "joins query key and value into a single string" (fun () -> 
      expect (Builder.to_query [("k1", "v1"); ("k2", "v2")] )
      |> toEqual ("?k1=v1&k2=v2")
    );
  );  

)