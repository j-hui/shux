type src_ctx = string * int


(* Scanner exceptions *)
exception IllegalCharacter of src_ctx * char
exception UnmatchedQuotation of src_ctx

