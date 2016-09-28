testwrite(O) :-
    open('output.txt',write,OS),
    string_to_list("hej", List),
    string_to_list(String, List),
    write(OS, String),
    O = String,
    close(OS).