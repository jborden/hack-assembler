# hack-assembler

An assembler for the hack assembly code language written in clojure

Written as part of the https://www.nand2tetris.org/ course

## Usage

$ lein repl
```clojure
hack-assembler.core> (-> (assemble-file "/Users/james/nand2tetris/projects/06/add/Add.asm"))
nil
```

will output the file "Add.hack" in the same directory as the asm file

## License

Copyright © 2020 James Borden

MIT License
