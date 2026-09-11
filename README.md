# simp-math

## build
```
elm make src/Main.elm --optimize --output assets/js/simp-math.js
```

For development, drop `--optimize` and serve the repo root with any static server,
e.g. `python3 -m http.server`, then open http://localhost:8000.

## deploy
GitHub Pages: Settings → Pages → Deploy from branch `master`, folder `/ (root)`.
Commit `assets/js/simp-math.js` after building.

