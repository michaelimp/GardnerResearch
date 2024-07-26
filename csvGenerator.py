# Code that generates the CSV files for querying mix probabilities and assignment rule
import operator as op
from functools import reduce
import numpy as np
import math
import pandas as pd
import sys
from parameters import *

# Parameters:
NUM_SERVERS = NUM_SERVERS
ARRIVAL_RATE = ARRIVAL_RATE
d = d
s = s
jobsAdded = 0
TOTAL_JOBS = TOTAL_JOBS
THRESHOLD = THRESHOLD
querying_file_path = "queryingRuleProbabilities.csv"
assignment_file_path = "assignmentRuleProbabilities.csv"

#______________________________________________________________________________________________
# Creating querying rule csv
def ncr(n, r): # pass in n = d + s - 1, r = s -1
    ''' The total number of possible query mixes is (d + s - 1 choose s - 1) '''
    r = min(r, n-r)
    numer = reduce(op.mul, range(n, n-r, -1), 1)
    denom = reduce(op.mul, range(1, r+1), 1)
    return numer // denom


def constrained_partitions(n, k, min_elem, max_elem):
    ''' https://stackoverflow.com/questions/58915599/generate-restricted-weak-integer-compositions-or-partitions-of-an-integer-n-in'''
    allowed = range(max_elem, min_elem-1, -1)

    def helper(n, k, t):
        if k == 0:
            if n == 0:
                yield t
        elif k == 1:
            if n in allowed:
                yield t + (n,)
        elif min_elem * k <= n <= max_elem * k:
            for v in allowed:
                yield from helper(n - v, k - 1, t + (v,))

    return helper(n, k, ())

def create2DProbabilityArray():
    '''Returns a 2D Numpy array where each row is the query mix with the last column as the corresponding probability of it occurring. The query mix goes from fastest to slowest per column index
    :rtype: 2D numpy array
    '''
    
    #Initializes the "first column" of query mixes
    A = np.zeros(s)
    for queryMix in constrained_partitions(d, s, 0, d):
        A = np.vstack([A, queryMix])
    A = np.delete(A,0,0)
    numQueryMixes = ncr(d+s-1,s-1)
    
    # Adds to "second" column the probability of each query mix, given no idle servers
    B = np.ones((numQueryMixes,1))
    i = 0
    for queryMix in constrained_partitions(d, s, 0, d):
        denominator = 1
        for index in queryMix:
            denominator *= math.factorial(index)
        probability = math.pow(1/s,d) * math.factorial(d) / denominator
        B[i] = probability
        i += 1
    A = np.hstack([A, B])
    
    # Adds the rest of the columns, corresponding to probability of each query mix, given a fastest idle server
    for fastestIdleServerSpeed in range(s-1, 0, -1):
        B = np.ones((numQueryMixes,1)) # Change so that each query mix could have different probabilities
        
        i = 0
        for queryMix in constrained_partitions(d, s, 0, d):
            probability = 2 # place holder
            unacceptableIndexes = [x for x in range(fastestIdleServerSpeed, s)]
            booleanFlag = False
            for unacceptableIndex in unacceptableIndexes:
                if queryMix[unacceptableIndex] != 0:
                    probability = 0
                    B[i] = probability
                    i += 1
                    booleanFlag = True
                    break
            if booleanFlag:
                continue
            denominator = 1
            for index in queryMix:
                denominator *= math.factorial(index)
            probability = math.pow(1/fastestIdleServerSpeed,d) * math.factorial(d) / denominator
            B[i] = probability
            i += 1
        A = np.hstack([A, B])
    return A

#______________________________________________________________________________________________
# Assignment hashmap
listOfKeys = [] # to aid in creation of assignment hashmap

def keyGen(qLengths, threshold, servClass, currThreshold):
    '''keyGen updates listOfKeys to contain all the possible keys. A key is vector of length s, where index 0 corresponds to fastest server class.
    Each index corresponds to the shortest queue length for each server class.
    Index 0 goes from 0 to threshold, and sys.max (which would occur when the fastest server class is never queried).
    Index 1 must be less than index 0, or it is sys.max
    An example key where threshold = 5, s = 4, would be: [5, sys.max, 1, 0]. Notice it is strictly decreasing
    An index could equal the global threshold, but always must be smaller than the local threshold (the local threshold as determined by the shortest length of servers faster than it)
    Return: nothing, just updates listOfKeys 
    '''
    if servClass == s: #base case
        #This step is necessary to handle issues with memory referencing: 
        deepCopy = []
        for i in qLengths:
            deepCopy.append(i)
        #__________________
        listOfKeys.append(deepCopy)
        return
    
    for i in range(threshold + 2):
        if i > currThreshold: 
            return
        if i == currThreshold:
            qLengths[servClass] = sys.maxsize
            localThreshold = currThreshold
        elif (i == threshold + 1):
            qLengths[servClass] = sys.maxsize
            localThreshold = currThreshold
        else:
            qLengths[servClass] = i
            localThreshold = qLengths[servClass]
        keyGen(qLengths,threshold,servClass+1, localThreshold)


def createAssignmentHashmap():
    ''' Currently implements a shortest queue length assignment rule
    Return: dictionary with keys as described above, and the value a probability distribution (vector of length s, sums to 1, at each index is 
    probability that that server class is assigned the job)
    rtype: dictionary
    '''
    qLengths = [0]*s
    keyGen(qLengths, THRESHOLD, 0, THRESHOLD+1) # creates listOfKeys
    assignmentHashmap = {}
    for key in listOfKeys:
        value = [0]*s # Probability distribution, vector [0,0...0] of length s
        value[key.index(min(key))] = 1 # Make the shortest queue length server class have probability 1 of being picked
        hashableKey = tuple(key) # list is non hashable, must convert to tuple
        assignmentHashmap[hashableKey] = value
    return assignmentHashmap

def createAssignmentKey(selectedQueryMix, queriedServers):
    ''' Creates assignment key based on the selected query mix that we'll hash into assignmentHashmap
    ptype: list of queriedServers
    return: assignment key
    rtype: list of len(s)'''
    assignmentKey = [0]*s 
    # Determine assignment key
    tracker = 0
    for i in range(s): # Puts shortest queue length of each server class into assignment key
        numThisClass = selectedQueryMix[i]
        if numThisClass == 0: # If that server class was not queried
            assignmentKey[i] = sys.maxsize
        else: # Finds the shortest queue length of the class
            queriedServersOfSameClass = queriedServers[int(tracker):int(tracker + numThisClass)]
            minLength = sys.maxsize
            for server in queriedServersOfSameClass:
                if len(server.queuedJobs) <= minLength:
                    minLength = len(server.queuedJobs)
            assignmentKey[i] = minLength
            tracker += numThisClass
    # Finishes fixing assignment key
    localThreshold = THRESHOLD + 1
    for i in range(len(assignmentKey)): 
        if assignmentKey[i] == sys.maxsize: # This means that server class was not queried, so we don't want to do anything
            continue
        if assignmentKey[i] >= THRESHOLD: # Treat any length above threshold as at threshold
            assignmentKey[i] = THRESHOLD
        if assignmentKey[i] >= localThreshold: # If equal or above local threshold, consider it as completely unviable
            assignmentKey[i] = sys.maxsize
        else: # This would mean we have new shortest length to determine local threshold
            localThreshold = assignmentKey[i] 
    return assignmentKey


def generate_querying_probability_csv():
    '''Generates our querying rule probability csv file'''
    global d, s
    two_D_Array = create2DProbabilityArray()
    # Save the 2D NumPy array as a CSV file
    np.savetxt(querying_file_path, two_D_Array, delimiter=',', fmt='%.5f')

def main():
    global d, s
    two_D_Array = create2DProbabilityArray()
    # Save the 2D NumPy array as a CSV file
    np.savetxt(querying_file_path, two_D_Array, delimiter=',', fmt='%.5f')
    assignmentHashmap = createAssignmentHashmap()
    df = pd.DataFrame(assignmentHashmap)
    df.to_csv(assignment_file_path, index = False)

if __name__ == '__main__':
    main()
    