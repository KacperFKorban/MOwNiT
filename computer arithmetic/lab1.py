# 1

import numpy as np

print(np.float64(1/3))

print(np.float32(1/3))

print(np.float64(np.float32(1)/np.float32(3)))

# 2

import matplotlib.pyplot as plt

def eps(x):
    return np.nextafter(x, x+1)-x

xs = np.linspace(1,1000,1000)
ys = eps(xs)

plt.plot(xs, ys, 'o')
plt.show()

# 3

# 4

def sigmoid(x):
    return 1. / (1 + np.exp(-x))

def sigmoid_stable(x):
    if x >= 0:
        return 1. / (1 + np.exp(-x))
    else:
        return np.exp(x) / (1. + np.exp(x))

x = -710

print("sigmoid", x+1, sigmoid(x+1))
print("sigmoid", x+1, sigmoid_stable(x+1))

print("sigmoid", x, sigmoid(x))
print("sigmoid", x, sigmoid_stable(x))