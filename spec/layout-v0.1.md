# Figure Layout v0.1

`layout` is a Scene3D figure-level protocol. It is not a theme object and it is
not a WebGL object.

The renderer must use one layout path for HTML preview and headless export:

```json
{
  "layout": {
    "plotMargin": {"top": 8, "right": 8, "bottom": 8, "left": 8, "unit": "px"},
    "titleArea": {"enabled": true},
    "legendArea": {
      "position": "right",
      "spacing": 12,
      "inside": {
        "position": [0.98, 0.98],
        "justification": [1, 1]
      }
    },
    "sceneViewport": {"fit": "remaining"}
  }
}
```

Rules:

- outside legends reserve fixed layout area;
- inside legends are positioned relative to the scene viewport and do not reserve area;
- WebGL renders only inside the scene viewport;
- title, subtitle, caption, legend, and scene viewport are siblings in the figure layout;
- `render` controls fixed export size, CSS size, pixel size, and device pixel ratio.

