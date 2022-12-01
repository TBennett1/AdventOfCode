def part1(data):
    lstOfIDs=[]
    for seat in data:
        row = list(seat[:7])
        col = list(seat[7:])
        numRow = [0,127]
        numCol = [0,7]

        for i in row:
            if i =='F':
                numRow[1] = (numRow[0]+(numRow[1]-1))//2
            elif i == "B":
                numRow[0] = (numRow[0]+(numRow[1]+1))//2
        
        for i in col:
            if i =="R":
                numCol[0] = (numCol[0]+(numCol[1]+1))//2
            elif i == "L":
                numCol[1] = (numCol[0]+(numCol[1]-1))//2
        
        lstOfIDs.append(numRow[0]*8+numCol[0])

    return(max(lstOfIDs))

def part2(data):
    lstOfIDs=[]
    for seat in data:
        row = list(seat[:7])
        col = list(seat[7:])
        numRow = [0,127]
        numCol = [0,7]

        for i in row:
            if i =='F':
                numRow[1] = (numRow[0]+(numRow[1]-1))//2
            elif i == "B":
                numRow[0] = (numRow[0]+(numRow[1]+1))//2
        
        for i in col:
            if i =="R":
                numCol[0] = (numCol[0]+(numCol[1]+1))//2
            elif i == "L":
                numCol[1] = (numCol[0]+(numCol[1]-1))//2
        
        lstOfIDs.append(numRow[0]*8+numCol[0])
    lstOfIDs.sort()
    return([myID for myID in range(lstOfIDs[0],lstOfIDs[-1]+1) if myID not in lstOfIDs])
        

#rows=128
#col=8
with open("input5.txt") as f:
    data = f.read().split('\n')

print(part1(data))
print(part2(data))