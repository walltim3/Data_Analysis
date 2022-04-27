import numpy as np
from collections import Counter

def ngram_ctr(letters):
        keys = Counter(letters).keys() #stores n-gram to keys
        values = [x/len(letters) for x in Counter(letters).values()] #counts corresponding frequencies
        #creating a new dict
        retval =  dict(zip(keys, values))
        return dict(sorted(retval.items(), key=lambda item: item[1], reverse=True)) #sorting dict from highest to lowest
