from itertools import accumulate, count

with open("day3Input.txt", "r") as f:
    c = f.read()
    wires = c.split("\n")

d = {"U": 1j, "R": 1, "D": -1j, "L": -1}

def manhattan(v):
    return abs(v.real) + abs(v.imag)

def steps(wires):
    step = (d[s[0]] for s in wires.split(",") for _ in range(int(s[1:])))
    return dict(reversed(list(zip(accumulate(step), count(1)))))

route_one = steps(wires[0])
route_two = steps(wires[1])
common_points = set(route_one.keys()) & set(route_two.keys())
closest = min(common_points, key= lambda p: manhattan(p))

#part 1
print(int(manhattan(closest)))

#part 2
distances = ((route_one[p], route_two[p]) for p in common_points)
print(int(min(map(sum, distances))))
