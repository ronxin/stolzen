import sqlite3
con = sqlite3.connect('db1.db')

def calc_aposteriori(res):
    prior = sum([r[1] for r in res])
    apost = {r[0]: r[1] / prior for r in res}
    return apost['y']

def calc(con, args, evidence):
    rest = ' and '.join(["%s='%s'" % (key, value) for (key, value) in evidence.items()])
    res = {}
    for q in args:
        query = "select %s, sum(p) from joint where " % q + rest + " group by %s;" % q
        cur = list(con.execute(query))
        res[q] = calc_aposteriori(cur)
    return sorted(res.items(), key=lambda x: -x[1])

calc(con, ['t', 'l', 'b'], {'a': 'y', 's': 'n', 'd': 'y'})
calc(con, ['t', 'l', 'b'], {'s': 'n', 'x': 'y'})