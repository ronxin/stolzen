import sqlite3

con = sqlite3.connect('db.db')


def calc(con, *args, **evidence):
	rest = ' and '.join(["%s='%s'" % (key, value) for (key, value) in evidence.items()])
	for (q in args):
		query = "select p from joint where " + rest + " and %s='y'" % q
		cur = con.execute(query)

