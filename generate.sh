#!/bin/bash

cd free-foil-sprite

echo "generating..."
bnfc --haskell -d -p Language.Sprite.Syntax --generic -o src/ grammar/Sprite/Front.cf
bnfc --haskell -d -p Language.Sprite.Syntax --generic -o src/ grammar/Sprite/Inner.cf

echo "use alex and happy for front"
cd src/Language/Sprite/Syntax/Front
alex Lex.x
happy --ghc Par.y


echo "use alex and happy for inner"
cd "../Inner"
alex Lex.x
happy --ghc Par.y

echo "finish generation"
