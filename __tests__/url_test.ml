open Jest
open Expect

let url protocol host port path query fragment : Url.url =
  { protocol; host; port; path; query; fragment }

let () = describe "Url" (fun () -> 
  describe "#from_string" (fun () -> 
    test "parse advanced string" (fun () -> 
      expect (Url.from_string "https://example.com:8042/over/there?name=ferret#nose") 
      |> toEqual(
        Some (url Url.Https "example.com" (Some 8042) "/over/there" (Some "name=ferret") (Some "nose")  )
      )
    );
    test "parse from basic string" (fun () -> 
      expect (Url.from_string "http://example.com") 
      |> toEqual(
        Some (url Url.Http "example.com" None "/" None None)
      )
    );
    test "fails when protocol missing" (fun () -> 
      expect (Url.from_string "example.com") 
      |> toEqual(None)
    );   
    test "fails when username present" (fun () -> 
      expect (Url.from_string "http://username@example.com") 
      |> toEqual(None)
    );  
    test "fails when host missing" (fun () -> 
      expect (Url.from_string "http://#cats") 
      |> toEqual(None)
    );      
  );

  describe "#of_string" (fun () -> 
    test "return string from advanced url" (fun () -> 
      expect (Url.of_string (url Url.Https "example.com" (Some 8042) "/over/there" (Some "name=ferret") (Some "nose"))) 
      |> toBe("https://example.com:8042/over/there?name=ferret#nose")
    );
    test "return string from basic url" (fun () -> 
      expect (Url.of_string (url Url.Http "example.com" None "/" None None))
      |> toBe("http://example.com/")
    );
  );
)