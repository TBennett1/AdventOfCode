def fuel(mass: list) -> int:
    ans=0
    for i in mass:
        ans += ((int(i)//3)-2)
    return ans

def fuel2(mass: int) -> int:
    ans = 0
    while mass >= 0 and (mass//3 >2):
        t = (mass//3)-2
        ans += t
        mass = t
    return ans



with open("day1Input.txt", 'r') as f:
    #print(fuel(f))
    result =0
    for i in f:
        i = int(i)
        result += fuel2(i)
    print(result)
