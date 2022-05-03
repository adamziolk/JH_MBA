import numpy as np
from scipy.stats import binom, norm
import matplotlib.pyplot as plt

# n, p = 20, .35
# x = np.arange(binom.ppf(0.01, n, p), binom.ppf(0.99, n, p))

# fig, ax = plt.subplots(1, 1)
# ax.plot(x, binom.pmf(x, n, p), 'bo', ms=8)
# plt.show()


# binom.pmf(10, n, p)
# binom.pmf(5, n, p)

# mu, std = 4500, 600
# x = np.linspace(norm.ppf(0.01, mu, std), norm.ppf(.99, mu, std), 1000)

# fig, ax = plt.subplots(1, 1)
# ax.plot(x, norm.pdf(x, mu, std))
# plt.show()

# 1-norm.cdf(5200, mu, std) # Probability of greater then 5200
# norm.cdf(4000, mu, std)
# norm.cdf(5400, mu, std) - norm.cdf(4200, mu, std)
# norm.cdf(5600, mu, std) - norm.cdf(4800, mu, std)



# mu, std = 15000, 2000

# norm.ppf(.15, mu, std)


n, p = 66, .5
x = np.arange(binom.ppf(0.01, n, p), binom.ppf(0.99, n, p))

fig, ax = plt.subplots(1, 1)
ax.plot(x, binom.pmf(x, n, p), 'bo', ms=8)
plt.show()


(1-binom.cdf(40, n, p)) * 60



n, p = 22, .5
x = np.arange(binom.ppf(0.01, n, p), binom.ppf(0.99, n, p))

fig, ax = plt.subplots(1, 1)
ax.plot(x, binom.pmf(x, n, p), 'bo', ms=8)
plt.show()


(1-binom.cdf(14, n, p)) * 50
