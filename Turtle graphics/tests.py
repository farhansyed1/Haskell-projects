# Program to generate tests in python
f = open("testghc2.txt","w")
s = "DOWN."

for i in range (1,10000):
    s += "REP 1 "
    s += "\n"

s+= " FORW 1. "

f.write(s)