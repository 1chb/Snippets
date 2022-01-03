# Check that there are no more than k distinct digits in the number n and the d least significant digits of n^2.  
 
def chk(n,d,k): 
 return len(set(str(n)+str((n*n)%(10**d)))) <= k 
 
# Start with numbers of at most j digits having at most 4 unique digits between themselves and the last j digits of their squares. 
 
s = [0]
 
# Extend each number from the previous step with all possible digits, confirming additional digits at each step 
 
for d in range(12):
 exp = 10**d 
 s = [b*exp+m for b in range(10) for m in s if chk(b*exp+m,d+1,4)] 
 
# Ensure that the full set of digits (between n and n^2) does not exceed 4 digits, now without restricting to the least 12 digits. 
 
ss=[n for n in s if chk(n,24,4)] 
print(len(ss))
# 10165
