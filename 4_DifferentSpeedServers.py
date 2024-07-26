import random
import numpy as np
import sys

# What if the variable servers is now a dictionary, where each key refers to class of server, and each value is a list of servers of that speed class

# Arrival rate and generateNewInterArrival is now a universal constant instead of part of the Server class.
class Server:
    def __init__ (self, speed):
        self.queuedJobs = []
        self.nextDepartureTime = sys.maxsize # Let each server hold onto its own next departure time (when its first job finishes). But the next arrival time is still universal. 
        self.speed = speed # Possible speed values: .5, 1, 1.5, 2
  
def generateNewInterArrival(arrival_rate):
        return np.random.exponential(1 / arrival_rate)
        
def generateNewJobSize(server_speed):
    return np.random.exponential(1/server_speed)

class Job:
    def __init__ (self, arrival_time, server_speed):
        self.arrival_time = arrival_time
        self.timeForCompletion = generateNewJobSize(server_speed)
    

def main():
    numServers = 1000
    numClassOfServers = 4
    
    servers = {}
    if numClassOfServers == 1:
        x = 1
    else:
        x = .5
    for i in range(numClassOfServers):
        listOfServers = []
        for j in range(numServers//numClassOfServers):
            listOfServers.append(Server(x))
        servers[i] = listOfServers
        x +=.5
    
    allServersAsList = sum(list(servers.values()),[]) # Complex code to handle the fact that servers is a dictionary
    
    arrival_rate = .5 * numServers
    d = 2
    
    
    clock = 0
    nextArrivalTime = generateNewInterArrival(arrival_rate)
    totalTime = 0 # captures sum of response time of all jobs
    
    jobsAdded = 0
    jobsToAdd = 1000000
    
    while jobsAdded < jobsToAdd:    
        clock = nextArrivalTime # Jump ahead to adding our next job
        
        queriedServers = random.sample(allServersAsList,d) 
        for server in queriedServers: # Finish up whatever jobs that should have been finished so the length of each queried server is accurate
            while True:
                if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
                    jobToFinish = server.queuedJobs.pop(0)
                    totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
                    if len(server.queuedJobs) != 0: # We process the next job
                        server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                    else: # We don't need to keep processing
                        server.nextDepartureTime = sys.maxsize
                else:
                    break
        
        # Select server with shortest length
        shortestServerLength = sys.maxsize
        shortestServerSpeed = 0
        server = queriedServers[0]
        for i in queriedServers:
            if len(i.queuedJobs) < shortestServerLength:
                shortestServerLength = len(i.queuedJobs)
                server = i
                shortestServerSpeed = server.speed
            if len(i.queuedJobs) == shortestServerLength and i.speed > shortestServerSpeed: # If server lengths are equal, pick the faster server
                shortestServerLength = len(i.queuedJobs)
                server = i
                shortestServerSpeed = server.speed
        
        while True:
            if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
                jobToFinish = server.queuedJobs.pop(0)
                totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
                if len(server.queuedJobs) != 0: # We process the next job
                    server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                else: # We don't need to keep processing
                    server.nextDepartureTime = sys.maxsize
            else:
                break
        
        newJob = Job(clock, server.speed)
        server.queuedJobs.append(newJob)
        if len(server.queuedJobs) == 1: # Begin working on the job if it's the only one
            server.nextDepartureTime = newJob.timeForCompletion + clock
            
        jobsAdded += 1
        nextArrivalTime += generateNewInterArrival(arrival_rate)
    
    # We need to finish up any remaining jobs in the servers
    serverNumber = 0
    for server in allServersAsList:
        while True:
            if server.nextDepartureTime < sys.maxsize - 100: # We need to first finish all the jobs that should have been completed by now in this server
                jobToFinish = server.queuedJobs.pop(0)
                # print("ServerNumber", serverNumber)
                # print(server.nextDepartureTime)
                totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
                if len(server.queuedJobs) != 0: # We process the next job
                    server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                else: # We don't need to keep processing
                    server.nextDepartureTime = sys.maxsize
            else:
                break
        serverNumber+=1
        
    print(totalTime/jobsToAdd)
    

    
if __name__ == '__main__':
    main()

    
    
 
# Uneducated code where the variable servers is not a dictionary, but a list of Server objects.
    
    
# import random
# import numpy as np
# import sys

# # Arrival rate and generateNewInterArrival is now a universal constant instead of part of the Server class.
# class Server:
#     def __init__ (self, speed):
#         self.queuedJobs = []
#         self.nextDepartureTime = sys.maxsize # Let each server hold onto its own next departure time (when its first job finishes). But the next arrival time is still universal. 
#         self.speed = speed # Possible speed values: .5, 1, 1.5, 2
  
# def generateNewInterArrival(arrival_rate):
#         return np.random.exponential(1 / arrival_rate)
        
# def generateNewJobSize(server_speed):
#     return np.random.exponential(1/server_speed)

# class Job:
#     def __init__ (self, arrival_time, server_speed):
#         self.arrival_time = arrival_time
#         self.timeForCompletion = generateNewJobSize(server_speed)
    

# def main():
#     servers = []
#     numServers = 1000
#     numClassOfServers = 4
    
#     for i in range(numServers // numClassOfServers):
#         servers.append(Server(.5))
#     for i in range(numServers // numClassOfServers, 2 * numServers // numClassOfServers):
#         servers.append(Server(1))
#     for i in range(2 * numServers // numClassOfServers, 3 * numServers // numClassOfServers):
#         servers.append(Server(1.5))
#     for i in range(3 * numServers // numClassOfServers, 4 * numServers // numClassOfServers):
#         servers.append(Server(2))
        
#     # Do later to account for different numClassOfServers
#     # if numClassOfServers == 1:
#     #     x = 1
#     # else:
#     #     x = .5
    
#     # for i in range(numServers):
#     #     if i >= numServers // numClassOfServers:
#     #         x += .5
#     #     servers.append(Server(x))
    
#     arrival_rate = .5 * numServers
#     d = 2
    
    
#     clock = 0
#     nextArrivalTime = generateNewInterArrival(arrival_rate)
#     totalTime = 0 # captures sum of response time of all jobs
    
#     jobsAdded = 0
#     jobsToAdd = 1000000
    
#     while jobsAdded < jobsToAdd:    
#         clock = nextArrivalTime # Jump ahead to adding our next job
        
#         queriedServers = random.sample(servers,d)
#         for server in queriedServers: # Finish up whatever jobs that should have been finished so the length of each queried server is accurate
#             while True:
#                 if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
#                     jobToFinish = server.queuedJobs.pop(0)
#                     totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
#                     if len(server.queuedJobs) != 0: # We process the next job
#                         server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
#                     else: # We don't need to keep processing
#                         server.nextDepartureTime = sys.maxsize
#                 else:
#                     break
        
#         # Select server with shortest length
#         shortestServerLength = sys.maxsize
#         server = queriedServers[0]
#         for i in queriedServers:
#             if len(i.queuedJobs) <= shortestServerLength:
#                 shortestServerLength = len(i.queuedJobs)
#                 server = i
        
#         while True:
#             if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
#                 jobToFinish = server.queuedJobs.pop(0)
#                 totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
#                 if len(server.queuedJobs) != 0: # We process the next job
#                     server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
#                 else: # We don't need to keep processing
#                     server.nextDepartureTime = sys.maxsize
#             else:
#                 break
        
#         newJob = Job(clock, server.speed)
#         server.queuedJobs.append(newJob)
#         if len(server.queuedJobs) == 1: # Begin working on the job if it's the only one
#             server.nextDepartureTime = newJob.timeForCompletion + clock
            
#         jobsAdded += 1
#         nextArrivalTime += generateNewInterArrival(arrival_rate)
    
#     # We need to finish up any remaining jobs in the servers
#     serverNumber = 0
#     for server in servers:
        
#         while True:
#             if server.nextDepartureTime < sys.maxsize - 100: # We need to first finish all the jobs that should have been completed by now in this server
#                 jobToFinish = server.queuedJobs.pop(0)
#                 # print("ServerNumber", serverNumber)
#                 # print(server.nextDepartureTime)
#                 totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
#                 if len(server.queuedJobs) != 0: # We process the next job
#                     server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
#                 else: # We don't need to keep processing
#                     server.nextDepartureTime = sys.maxsize
#             else:
#                 break
#         serverNumber+=1
        
#     print(totalTime/jobsToAdd)
    

    
# if __name__ == '__main__':
#     main()

    
    
    
    
    