npm run build:translations && elm make src/Main.elm --debug --output public/elm.bundle.js && \
esbuild src/Main.ts --bundle --sourcemap --outfile=public/main.bundle.js --minify --target=esNext