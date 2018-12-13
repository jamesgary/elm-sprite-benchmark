#!/bin/bash
echo "Make sure elm-live isn't running or it may override main.js while deploying!"

elm make src/Main.elm --optimize --output=public/js/main.js

# make fresh gh-pages branch
git branch -D gh-pages
git checkout --orphan gh-pages
git add -f public/js/main.js
git commit -m "Deployyy"
git push origin :gh-pages && git subtree push --prefix public origin gh-pages
git checkout master
