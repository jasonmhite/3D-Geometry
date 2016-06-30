import cPickle as pkl
import json

with open("spec.pkl") as f:
    geo, _, _ = pkl.load(f)

vertices = [
    {
        "id": i,
        "points": list(p.exterior.coords)[:-1],
    }
    for (i, p) in enumerate(geo.domain.geometry)
]

with open("vertices.json", "w") as out:
    json.dump(
        vertices,
        out,
        indent=2,
    )
