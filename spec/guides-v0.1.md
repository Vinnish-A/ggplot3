# Guides v0.1

Guides are top-level Scene3D overlay specs. They are not WebGL layers and do not
participate in 3D bounds.

Discrete legend:

```json
{
  "id": "guide-colour-class",
  "type": "legend",
  "aesthetic": "colour",
  "title": "class",
  "order": 1,
  "entries": [
    {
      "label": "compact",
      "value": "#C49A00",
      "glyph": {"type": "point", "colour": "#C49A00", "size": 4, "alpha": 1}
    }
  ],
  "materialMode": "unlit"
}
```

Continuous colourbar:

```json
{
  "id": "guide-colour-density",
  "type": "colorbar",
  "aesthetic": "colour",
  "title": "density",
  "domain": [0, 1],
  "palette": ["#2166AC", "#F7F7F7", "#B2182B"],
  "materialMode": "unlit"
}
```

Rules:

- R owns scale training and guide semantics;
- projection decorator layers must not create duplicate guides;
- `materialMode` records whether lighting affects perceived colour;
- renderer draws guides in overlay space and includes them in `ggsave3()` exports.

