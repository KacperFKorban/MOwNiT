import numpy as np

def dft(x):
    N = x.shape[0]
    n = np.arange(N)
    k = n.reshape((N, 1))
    M = np.exp(-2j * np.pi * k * n / N)
    return np.dot(M, x)

def fft(x):
    n = x.shape[0]
    if n <= 32:
        return dft(x)
    else:
        x0 = fft(x[::2])
        x1 = fft(x[1::2])
        factor = np.exp(-2j * np.pi * np.arange(n) / n)
        return np.concatenate([x0 + factor[:n // 2] * x1, x0 + factor[n // 2:] * x1])
