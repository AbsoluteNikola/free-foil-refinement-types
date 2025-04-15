#!/bin/bash

cd free-foil-sprite

echo "generating..."
bnfc --haskell -d -p Language.Sprite.Syntax --generic -o src/ grammar/Sprite/Front.cf
bnfc --haskell -d -p Language.Sprite.Syntax --generic -o src/ grammar/Sprite/Inner.cf

echo "use alex and happy for front"
cd src/Language/Sprite/Syntax/Front
alex Lex.x
happy --ghc  Par.y


echo "use alex and happy for inner"
cd "../Inner"
alex Lex.x
happy --ghc Par.y

cd ../../../../../..

cd free-foil-refinements

bnfc --haskell -d -p Language.Refinements --generic -o src/ grammar/Predicates.cf

echo "use alex and happy for predicates"
cd src/Language/Refinements/Predicates
alex Lex.x
happy --ghc Par.y

echo "finish generation"
