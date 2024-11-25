#!/bin/bash

cd free-foil-sprite

bnfc --haskell -d -p Language.Sprite --generic -o src/ grammar/Sprite/Syntax.cf
cd src/Language/Sprite/Syntax
alex Lex.x
happy Par.y
