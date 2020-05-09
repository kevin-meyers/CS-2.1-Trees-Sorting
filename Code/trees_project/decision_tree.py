import numpy as np
from collections import Counter


def entropy_of(p):
    return np.sum([-i*np.log(i) for i in p])

def probabilities_for(items):
    c = Counter(items)
    return np.array([v/len(items) for _, v in c.items()])

def gini_index_of(p):
    return 1 - np.sum(np.square(p))
