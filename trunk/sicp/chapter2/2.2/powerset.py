
def powset2(seq):
    if seq:
        head, tail = seq[:1], seq[1:]
        res = powset2(tail)
        return res + map(lambda s: head + s, res)
    else:
        return [[]]

print powset2([1, 2, 3])