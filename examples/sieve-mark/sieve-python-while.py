#limit = 5000000
limit = 500000
vec = range(0, limit)

vec[0] = 0
prime_count = 0

# This will eat all your swap and then die miserably. Python 'for' loops
# suck very, very badly. (Yes, this *is* the official idiom!)
#for i in range(0, limit - 1):
#    if vec[i] != 0:
#        prime_count = prime_count + 1
#        prime = i + 1
#        for j in range(i + prime, limit - 1, prime):
#            vec[j] = 0

i = 0
while i < limit:
    if vec[i] != 0:
        prime_count = prime_count + 1
        prime = i + 1
        j = i + prime
        while j < limit:
            vec[j] = 0
            j = j + prime
    i = i + 1

print "There are ", prime_count, " primes less than or equal to ", limit, "."
