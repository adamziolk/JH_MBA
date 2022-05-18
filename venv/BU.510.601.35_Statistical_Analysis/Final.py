# -*- coding: utf-8 -*-
"""
Created on Tue Dec  7 16:26:58 2021

@author: Adam-PC
"""
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
import math

# The mean amount of purchase by 25 typical customers at the Clinton Grocery Store is
# $23.50, with a standard deviation of 3.50.  Assuming a normal distribution the 90% interval is:
mu, sig = 23.5, 3.5

x = np.linspace(norm.ppf(0.01, mu, sig), norm.ppf(0.99, mu, sig), 10000)
plt.plot(x, norm.pdf(x, mu, sig))

t = 1.711
n = 25
mu - t*(sig/math.sqrt(n))
mu + t*(sig/math.sqrt(n))


#Zappos thinks that the average weight of outgoing
#orders is less than 6 lbs.
#A sample of 25 packages are weighted and
#found to weight 5.95 lbs with a
# sample standard deviation is 0.6.
#Use hypothesis testing at the 5% level and select the best answer.

mu, sig = 5.95, 0.6

t = 1.711
n = 25

mu - t*(sig/math.sqrt(n))

# H_0: mu >= 6

z = 1.54
mu - z*(sig/math.sqrt(n))


# Q6
p = .2
p_hat = 14/85
n = 85

sig  = math.sqrt( (p*(1-p)) / n)

# Crit p
z = 2.58
#crit_val =
p - z*sig
p + z*sig


# Q 9
h_0 = 72_500
n = 144
mu = 71_500
sig = 4_500

z = 1.96

#lb
lb = h_0 - z*(sig / math.sqrt(n))
# ub
ub = h_0 + z*(sig / math.sqrt(n))

x = np.linspace(norm.ppf(0.01, h_0, sig), norm.ppf(0.99, h_0, sig), 10000)
plt.plot(x, norm.pdf(x, h_0, sig))
left = norm.cdf(lb, h_0, sig)
left/1

norm.ppf(.0076, h_0, sig)
norm.pdf(mu, h_0, sig/math.sqrt(n))
norm.pdf(mu, h_0, sig)


import scipy.stats
? scipy.stats.f_oneway
scipy.stats.f_oneway(h_0, mu)
