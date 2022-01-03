### Goal

The Real Virtuality engine, which serves as the backbone for majority of Bohemia Interactive games, gives 3rd party
developers the ability to mod the games through the scripting language SQF. The success of the Arma can be attributed to its moddability, and it is a crucical aspect of the game.

However, SQF is a severely flawed langauge. It has a strange and inconsistent syntax, it has limited capacities, and is difficult and frustrating to develop, test and debug. From years of experience, the community has seen that it will remain this way, and that BI developers seemingly have little to no interest in improving this experience.

For this reason, this language was designed. The aim was to create a superior, statically typed, and modern, that is converted to SQF. The compiler will handle optimizations, package managment, ßproject structuring and support for writing extensions in Go. This project will remain open-source, allowing the community to decide upon changes in the language, aswell as making it easier to compile to another source target, for example the Enforce language, used in the new Enfusion engine.

### Terms

Lexeme - Value
Token - Containing lexeme and token type, aswell as location information on token
Lexer - Tokenizer, scanner
Parser - Takes stream of tokens and constructs a parse tree

### Lexer

### Parser