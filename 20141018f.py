import matplotlib.pyplot as plt
import numpy as np
import math

#f = np.array([[9], [5], [3], [1]])
f = np.array([[7], [7], [2], [2]])
x = np.array([1,2,3,4])

a = np.array([[1,1,0,1]
              ,[1,1,1,0]
              ,[0,1,1,1]
              ,[1,0,1,1]])

a = np.array([[1,1,0,1]
              ,[0,1,0,0]
              ,[0,0,1,0]
              ,[1,0,1,1]])

a = np.array([[-2, 1, 0, 1]
              ,[0, 1, 0, 0]
              ,[0, 0, 1, 0]
              ,[1, 0, 1, -2]])

q, r = np.linalg.qr(a)
q = -1*q


#アダマール変換
wh = np.array([[1,1,1,1]
               ,[1, -1, 1, -1]
               ,[1,1,-1, -1]
               ,[1, -1, -1,1]])

wq, wr= np.linalg.qr(wh)
wq = -1*wq

#Haar 基底
ht = np.array([[1/2, 1/2, 1/2, 1/2]
               ,[1/2,1/2,-1/2,-1/2]
               ,[1/math.sqrt(2),-1/math.sqrt(2), 0, 0]
               ,[0, 0, 1/math.sqrt(2), -1/math.sqrt(2)]])
hq = ht
#hq, hr = np.linalg.qr(ht.T)
#hq = -1 * hq


my = q.dot(hq)
mq, mr= np.linalg.qr(my)
mq = -1*mq

plt.plot(x, f, label='f')
plt.plot(x, q.T.dot(f), label="Q^t f")
plt.plot(x, hq.T.dot(f), label='H^t f')
#plt.plot(x, wq.T.dot(q.T.dot(f)))
plt.plot(x, mq.T.dot(f), label="M^t f")

plt.legend()
plt.grid()
plt.show()

