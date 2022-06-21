def insertEnd(a,ar):
    if ar == []:
        ar = [a]
    else: 
        return [ar[0]] + insertEnd(a,ar[1:])

print(insertEnd(9,[1,2,3,4]))
