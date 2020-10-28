# Honey Time
Graphical clock-calendar for the Sajem Tan project.

## Installation
The dynamic elements of the page are implemented in PureScript using the Spago
package manager / built tool. You can install these with npm like so:
```
npm install -g purescript purescript-spago
```

To compile and bundle the PureScript, use:
```
spago bundle-app
```

You can then run the project locally using simplehttpserver (direct your
browser to `http://localhost:8000` to view):
```
npm install -g simplehttpserver
simplehttpserver
```
