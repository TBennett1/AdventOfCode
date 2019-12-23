def passCount(min,max):
    count=0
    for i in range(min, max+1):
        currNum = [int(x) for x in str(i)]
        if currNum == sorted(currNum) and not (len(currNum)==len(set(currNum))):
            count += 1
    return count

def has_double(num):
    for i in range(5):
        if num[i] == num[i+1] and (i == 0 or num[i] != num[i-1]) and (i == 4 or num[i] != num[i+2]):
            return True
    return False

def passCount2(min, max):
    count=0
    for i in range(min, max+1):
        currNum = [int(x) for x in str(i)]
        if currNum == sorted(currNum) and has_double(currNum):
            count += 1
    return count

#part 1
print(passCount(245318,765747))

#part 2
print(passCount2(245318,765747))
