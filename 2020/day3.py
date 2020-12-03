def part1(data):
    i=0 #row
    j=0 #col
    numTrees = 0
    while i < len(data):
        currPos = data[i][j]
        if currPos == '#':
            numTrees+=1
        i+=1
        j+=3
        if i<len(data) and j >= len(data[i]):
            k=i
            for row in data[i:]:
                data[k].extend(data[k])
                k+=1
    return numTrees

def part2(data,right,down):
    i=0 #row
    j=0 #col
    numTrees = 0
    while i < len(data):
        currPos = data[i][j]
        if currPos == '#':
            numTrees+=1
        i+=down
        j+=right
        if i<len(data) and j >= len(data[i]):
            k=i
            for row in data[i:]:
                data[k].extend(data[k])
                k+=1
    return numTrees



with open("input3.txt") as f:
    rawData = f.read().split()
    data = [list(x) for x in rawData]

print(part1(data))
slopes=[(1,1),(3,1),(5,1),(7,1),(1,2)]
total = 1
for slope in slopes:
    total*=part2(data,slope[0],slope[1])
print(total)