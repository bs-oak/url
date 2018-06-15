open Jest
open Expect
open Parser

let () = describe "Parser" (fun () -> 
  let default_url : Url.url =
    { protocol = Url.Http
    ; host = "example.com"
    ; port = None
    ; path = "/"
    ; query = None
    ; fragment = None 
    }
  in

  describe "#string" (fun () -> 
    test "returns existing path segment" (fun () -> 
      expect (parse string {default_url with path = "/hello"}) |> toEqual( Some "hello" )
    );
    test "returns existing path segment with trailing slash" (fun () -> 
      expect (parse string {default_url with path = "/hello/"}) |> toEqual( Some "hello" )
    );
    test "returns nothing on empty path" (fun () -> 
      expect (parse string {default_url with path = "/"}) |> toEqual( None )
    );    
  );

  describe "#int" (fun () -> 
    test "returns existing int path segment" (fun () -> 
      expect (parse int {default_url with path = "/123"}) |> toEqual( Some 123 )
    );
    test "returns existing int path segment with trailing slash" (fun () -> 
      expect (parse int {default_url with path = "/123/"}) |> toEqual( Some 123 )
    );
    test "returns nothing on empty path" (fun () -> 
      expect (parse int {default_url with path = "/"}) |> toEqual( None )
    );    
  );

  describe "#s" (fun () -> 
    test "parses matching path segment" (fun () -> 
      expect ((parse (map true  (s "blog")) ) {default_url with path = "/blog"}) |> toEqual( Some true )
    ); 
    test "stops parsing on non-matching path segment" (fun () -> 
      expect ((parse (map true  (s "bad_path")) ) {default_url with path = "/blog"}) |> toEqual( None )
    );
  );

  describe "#</>" (fun () -> 
    test "parses slashes in the path" (fun () -> 
      expect (parse (s "hello" </> string ) {default_url with path = "/hello/world"}) |> toEqual( Some "world" )
    );
  );

  describe "#map" (fun () -> 
    test "maps value" (fun () -> 
      expect ((parse (map true  (s "blog")) ) {default_url with path = "/blog"}) |> toEqual( Some true )
    ); 
  );

  describe "#one_of" (fun () -> 
    test "returns a value from the first parser that matches the url" (fun () -> 
      let parser =
        one_of 
          [ s "hello" </> string
          ; s "ohayou" </> string
          ; s "bonjour" </> string
          ]
      in
      expect (parse parser {default_url with path = "/ohayou/wakatta"}) |> toEqual( Some "wakatta" )
    ); 
  );

  describe "#top" (fun () -> 
    test "matches the top level path" (fun () -> 
      let parser =
        one_of 
          [ map "top" top
          ; s "ohayou" </> string
          ; s "bonjour" </> string
          ]
      in
      expect (parse parser {default_url with path = "/"}) |> toEqual( Some "top" )
    ); 
  );

  describe "#<?>" (fun () -> 
    test "matches query params" (fun () -> 
      let parser = query (Parser.Query.string "key") in
      expect (parse parser {default_url with query = Some "key=val"}) |> toEqual( Some (Some "val") )
    ); 
  );  

  describe "#fragment" (fun () -> 
    test "matches fragment" (fun () -> 
      let parser = fragment (fun x -> x) in
      expect (parse parser {default_url with fragment = Some "myfrag"}) |> toEqual( Some (Some "myfrag") )
    ); 
  );

  describe "#parse_hash" (fun () -> 
    test "parses fragment portion of url" (fun () -> 
      expect (parse_hash string {default_url with fragment = Some "/hello"}) |> toEqual( Some "hello" )
    );
  );
)