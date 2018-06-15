open Jest
open Expect

let url protocol host port path query fragment : Url.url =
    { protocol; host; port; path; query; fragment }

let () = describe "String" (fun () -> 
    describe "#is_empty" (fun () -> 
        test "returns true on blank string" (fun () -> 
            expect (Ext.String.is_empty "") 
            |> toBe(true)
        );
        test "returns false on non-blank string" (fun () -> 
            expect (Ext.String.is_empty "hello") 
            |> toBe(false)
        );
    );

    describe "#is_prefix" (fun () -> 
        test "returns true when value is prefix" (fun () -> 
            expect (Ext.String.is_prefix "hello world" "hell") 
            |> toBe(true)
        );
        test "returns false when not prefix" (fun () -> 
            expect (Ext.String.is_prefix "hello world" "abc") 
            |> toBe(false)
        );                  
    );

    describe "#drop_left" (fun () -> 
        test "drops specified number of chars from the left of the string" (fun () -> 
            expect (Ext.String.drop_left 4 "helloWorld") 
            |> toBe("oWorld")
        );    
        test "returns empty string when number of chars to be dropped is equal to the length of the string" (fun () -> 
            expect (Ext.String.drop_left 5 "hello") 
            |> toBe("")
        );                
        test "returns empty string when number of chars to be dropped is larger than string" (fun () -> 
            expect (Ext.String.drop_left 99 "helloWorld") 
            |> toBe("")
        );             
    );    

    describe "#indexes" (fun () -> 
        test "returns indexes of character in a string" (fun () -> 
            expect (Ext.String.indexes "hello world" 'l')  
            |> toEqual([2;3;9])
        );               
    );    

    describe "#to_int" (fun () -> 
        test "returns some int on int string" (fun () -> 
            expect (Ext.String.to_int "33")  
            |> toEqual(Some 33)
        );      
        test "returns none on non-int string" (fun () -> 
            expect (Ext.String.to_int "hello")  
            |> toEqual(None)
        );                    
    );  

    describe "#left" (fun () -> 
        test "returns a number of character from the left of the string" (fun () -> 
            expect (Ext.String.left "hello world" 4)  
            |> toBe("hell")
        );
    );  

    describe "#join" (fun () -> 
        test "joins a list of string with delims between each elements" (fun () -> 
            expect (Ext.String.join "&" ["cat"; "dog"; "bat"])  
            |> toBe("cat&dog&bat")
        );
    );      
)