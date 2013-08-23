# 6

ph1 = 0.55
n1 = 5
ph2 = 0.42
n2 = 8

cl = 0.95
alpha = (1 - cl) / 2

z = qnorm(alpha, lower.tail=F)

v = function(phat, n) phat * (1 - phat) / n
ME = z * sqrt(v(ph1, n1) + v(ph2, n2))

ph1 - ph2 + c(-ME, ME)



#9
# two independent samples
x1 = 12
s1 = 10
n1 = 500

x2 = 10
s2 = 15
n2 = 700

cl = 0.95
alpha = (1 - cl) / 2

df = (s1 / n1 + s2 / n2)^2 / ( (s1/n1)^2 / (n1 - 1) + (s2/n2)^2 / (n2 - 1) )
ts = sqrt(s1/n1 + s2/n2)
t = qt(alpha, df=df, lower.tail=F)
ME = t * ts

x1 - x2 + c(-ME, ME)




#10
# p-value unpooled
x1 = 10
s1 = 10
n1 = 25

x2 = 20
s2 = 15
n2 = 40

z = (x1 - x2) / sqrt(s1/n1 + s2/n2)
z

# p-value pooled
x1 = 10
s1 = 100
n1 = 250

x2 = 20
s2 = 150
n2 = 400

s = ((n1 - 1) * s1 + (n2 - 2) * s2) / (n1 + n2 - 2)
z = (x1 - x2) / sqrt(s/n1 + s/n2)
z