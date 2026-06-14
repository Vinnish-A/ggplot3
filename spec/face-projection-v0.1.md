# Face Projection v0.1

Stage 3 face projection layers carry their own 2D data. Stage 4 adds a
source-linked point projection decorator for large point clouds.

Independent face projection:

```json
{
  "type": "face_projection",
  "plane": "zmin",
  "axes": ["x", "y"],
  "data": {"kind": "grid2d", "encoding": "json-grid"},
  "style": {"type": "density_grid", "material": "unlit"}
}
```

Source-linked point projection:

```json
{
  "type": "face_projection",
  "sourceLayerId": "layer-1",
  "faces": ["xy_min", "xz_min", "yz_max"],
  "data": {"kind": "source_point_cloud", "encoding": "source-reference"},
  "style": {
    "type": "source_points",
    "alphaMultiplier": 0.45,
    "sizeMultiplier": 0.8,
    "offset": 0.0001
  },
  "guide": {"show": false}
}
```

Rules:

- source-linked projection does not duplicate source point data in Scene3D JSON;
- projection faces are explicit;
- projection style controls alpha, size, depth write, and z-fighting offset;
- source-linked projection does not affect scene bounds;
- source-linked projection does not create a guide.

