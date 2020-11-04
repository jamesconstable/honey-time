# Honey Time
Graphical clock-calendar for the Sajem Tan project.

## Build instructions
The dynamic elements of the page are implemented in PureScript using the Spago
package manager / built tool. You can install these with npm like so:
```
npm install -g purescript purescript-spago
```

The SVG images are generated from the Haskell code in the `svg-generation`
directory, using the Stack build tool. Ensure you have both Haskell and Stack
installed before continuing. If you use Homebrew on Mac, you can do this with:
```
brew install haskell-stack
```

To generate the site, run
```
./build.sh
```
in this directory; it will produce a new subdirectory called `site` containing
the results. You can deploy this directory directly as a static site, or run it
locally using simplehttpserver (direct your browser to `http://localhost:8000`
to view):
```
npm install -g simplehttpserver
cd site
simplehttpserver
```
