# -*- coding: utf-8 -*-
"""
Created on Mon Nov 15 18:36:12 2021

@author: Adam-PC
"""
from scipy.stats import binom, norm
import matplotlib.pyplot as plt
import numpy as np

# n, p = 6, .3
# x = np.arange(7)

# plt.plot(x, binom.pmf(x, n, p), 'bo')

# binom.cdf(5, n, p) - binom.cdf(1, n, p)


# n, p = 12, .1

# binom.cdf(0, n, p)


mu, sig = 27500, 4355
x = np.linspace(norm.ppf(0.01, mu, sig), norm.ppf(0.99, mu, sig))

plt.plot(x, norm.pdf(x, mu, sig))

norm.ppf(0.08, mu, sig)

norm.cdf(21380.9, mu, sig)
