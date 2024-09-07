import random
import numpy as np
import sys

#https://www.youtube.com/watch?v=0osGrraoCX0&t=2s
#https://www.youtube.com/watch?v=NypbxgytScM&t=4s

#if the job arrives, the clock jumps to arrival time, I need to create next arrival time. If the queue is empty, I need to also create a departure time
#if a job departs, I need to create a new departure time for the next job to be finished 

def generateNewJobSize(x):
    return np.random.exponential(x)

class MM1Queue:
    def __init__ (self, arrival_rate):
        self.arrival_rate = arrival_rate
        self.queuedJobs = []
    
    def generateNewInterArrival(self):
        return np.random.exponential(1 / self.arrival_rate)
    
   
    
class Job:
    def __init__ (self, arrival_time):
        self.arrival_time = arrival_time
        self.timeForCompletion = generateNewJobSize(1)
    


def main():
    queue = MM1Queue(.5)
    clock = 0
    
    nextArrivalTime = queue.generateNewInterArrival()
    nextDepartureTime = sys.maxsize
    
    totalTime = 0 # captures sum of response time of all jobs
    
    jobsCompleted = 0
    jobstoComplete = 100000
 
    while jobsCompleted != jobstoComplete:
        if nextArrivalTime <= nextDepartureTime: # if a job will first arrive before the current job in server will be finished
            clock = nextArrivalTime
            newJob = Job(clock)
            if len(queue.queuedJobs) == 0: # if there are no jobs running in the server, then the newly arrived job needs to start working 
                nextDepartureTime = newJob.timeForCompletion + clock
            queue.queuedJobs.append(newJob)
            nextArrivalTime = queue.generateNewInterArrival() + clock 
        else: # We will first finish a job before a new job arrives
            jobsCompleted += 1
            jobToFinish = queue.queuedJobs.pop(0)
            clock = nextDepartureTime
            totalTime += (clock - jobToFinish.arrival_time)
            if len(queue.queuedJobs) != 0:
                nextDepartureTime = queue.queuedJobs[0].timeForCompletion + clock
            else:
                nextDepartureTime = sys.maxsize
    print(totalTime/jobstoComplete)
    
    
    
if __name__ == '__main__':
    main()


    
    
    
    
    
    
    