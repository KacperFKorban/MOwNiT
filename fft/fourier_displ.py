import pandas as pd
import numpy as np
import scipy as sp
import datetime
from scipy.fftpack import fft, fftfreq
import matplotlib.pyplot as plt

df = pd.read_csv('data.csv', parse_dates=['Datetime'])

# df.plot(x='Datetime', y='AEP_MW')

# plt.show()

df['Datetime'] = df['Datetime'].apply(lambda x: x.timestamp())

n = df.shape[0]

freqs = fftfreq(n)

df_fft = fft(df['AEP_MW'])

plt.plot(freqs[1:n//2], np.abs(df_fft)[1:n//2])
plt.xlabel('Frequency /hour')
plt.ylabel('Frequency Domain (MW)')

plt.show()

"""Po zbadaniu transformaty dla zużycia prądu w USA,
można wysunąć wnioski, że największe zmiany mają miejsce
dobowo oraz kwartalnie"""
