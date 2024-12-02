import numpy as np
import requests

r = requests.get("https://adventofcode.com/2021/day/7/input",headers={'Cookie': '_ga=GA1.2.476278310.1638606484;_gid=GA1.2.1700296996.1638606484;session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46'})
v = np.array(list(map(int, r.text.split(','))))
m = np.arange(min(v),max(v)+1)
f = lambda m : np.sum(np.power((m-v),2)+np.abs(m-v))/2
#fm = np.vectorize(f)(m)
#np.savetxt("m.csv",np.vstack((m,fm)).T,delimiter=',')
av = np.mean(v)
print(np.floor(av),f(np.floor(av)))
print(np.ceil(av),f(np.ceil(av)))







