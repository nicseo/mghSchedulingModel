"""
Created on Thursday, September 18, 2014
Modified on 10/15

@author: nicseo

This script models efficient scheduling of prime time procedures in both Cath and EP based
on data taken over a 125 day (25 week) period. The constraints of the model are:
    1. Room location (where the procedure must be done)
    2. Scheduling horizon (the time period during which the procedure must be scheduled)
    3. Crossover policy (whether or not procedures are allowed to crossover between lab rooms)
    4. Week pairing policy (whether or not same week procedures can be scheduled over a two week span)
    5. Day pairing policy (whether or not same day procedures can be scheduled over a two day span: M,T/W,R/F)
    6. Same day only policy (whether or not procedures are scheduled on the same day they were historically or
       based on their scheduling horizon)
    7. Random post procedure time policy (whether or not to assign a random post procedure time from a distribution
       with a given mean and standard deviation)
    8. Resolution of holding bay time bins
    9. Priority policy (whether to place longest procedures first, shortest first, or as the input data stands)

Notes about use:
- The first part of this script sets up the necessary functions to carry out the analysis.
    At the bottom of this file, in the __main__ function, there are defined some variables
    that the user should verify before running the script (e.g. regarding the csv that the
    data is being read from).

- Results are output to a csv, whose name should be specified before running the script.

- Overflow procedures that are emergencies/same day procedures are listed during the day they
    were supposed to be scheduled. Overflows that were supposed to be scheduled within a week
    are listed in the first day of the week's overflows (Monday).
"""
import csv
import os
import copy
import random
import math

######################################################################################################
######################################################################################################
######################################### TIME PERIOD DATA TYPE ######################################
######################################################################################################
###################################################################################################### 

random.seed(30)

class TimePeriod:
    '''
    Class to model a given time period of scheduling.
    
    Initialization:
        TimePeriod(days,numCathRooms,numEPRooms)
            days - the number of days in the time period
            numCathRooms - the number of Cath rooms to be modeled
            numEPRooms - the number of EP rooms to be modeled

    User methods:
        packBins(procedures)
        
    Observer methods:
        sumProcTimes(dataList)
        getTotalMinutesInLabForDay(day,lab)
        getTotalMinutesInLabForWeek(week,lab) 

    Helper methods (not to be explicitly called outside of class):
        tryPlaceProcInLabDay(procedure,lab,day,nextOpenRoom,openRooms)
        packBinsForDay(day,daysProcedures)
        tryPlaceProcInLabWeek(procedure,lab,week,nextOpenRoom,openRooms)
        packBinsForWeek(week,weeksProcedures)
        
    '''
    
    def __init__(self,days,numCathRooms,numEPRooms,numRestrictedCath,numRestrictedEP,labStartTime):

        CathRooms = {(d,cathID,i):[] for i in xrange(numCathRooms) for d in xrange(days)}
        EPRooms = {(d,epID,i):[] for i in xrange(numEPRooms) for d in xrange(days)}
        rooms = dict(CathRooms.items() + EPRooms.items())
        overflow = {d:[] for d in xrange(days)}
        multiple = 60.0/resolution
        holdingBays = {(d,i/multiple):0 for i in xrange(0,int(HBCloseTime*multiple)) for d in xrange(days)}

        self.bins = [copy.deepcopy(rooms),copy.deepcopy(overflow),copy.deepcopy(holdingBays)]

        self.numCathRooms = numCathRooms
        self.numEPRooms = numEPRooms
        self.numRestrictedCath = numRestrictedCath
        self.numRestrictedEP = numRestrictedEP
        self.numDays = days
        self.numWeeks = days/5
        self.labStartTime = labStartTime
        self.numTotalProcs = None
        self.numSameDays = None
        self.numSameWeeks = None
        self.numEmergencies = None

        # statistical counters
        self.procsPlaced = 0
        self.procsPlacedData = []
        self.crossOverProcs = 0
        self.cathToEP = 0           # procedures historically done in Cath that are scheduled in an EP room
        self.epToCath = 0           # procedures historically done in EP that are scheduled in a Cath room
        self.overflowCath = 0
        self.overflowEP = 0
        self.overflowWeeks = []
        self.overflowDays = []
        

    ##################################### BIN PACKING FOR #####################################
    #################################### WHOLE TIME PERIOD ####################################

    def packBins(self,procedures,algType,weekPairs,dayPairs):
        '''
        Schedules procedures into the time period.
        
        Input: procedures (a list of cleaned procedure data for a given period of time)
                algType (a string describing the type of scheduling algorithm to run)
        Returns: none
        '''

        allProcs = procedures[:]

        # add procedure ID's
        for i in xrange(len(allProcs)):
            proc = procedures[i]
            proc.append(i)
                
        if sameDaysOnly:
            # change all same week procedures to same day
            for proc in allProcs:
                original = proc[iSchedHorizon]
                proc[iSchedHorizon] = 2.0 if original==3.0 else original
                
        if postProcRandom:
            # change the post procedure time to a random value from a distribution with a given mean/standard deviation
            for proc in allProcs:
                postTime = random.gauss(desiredMean, desiredStDev)
                proc[iPostTime] = postTime

        # break procedures up by scheduling horizon
        emergencies = [x for x in allProcs if x[iSchedHorizon]==1.0]
        sameDay = [x for x in allProcs if x[iSchedHorizon]==2.0]
        sameWeek = [x for x in allProcs if x[iSchedHorizon]==3.0]

        self.numTotalProcs = len(emergencies)+len(sameDay)+len(sameWeek)
        self.numSameDays = len(sameDay)
        self.numSameWeeks = len(sameWeek)
        self.numEmergencies = len(emergencies)
        
        # SAME WEEK procedures: two week spans
        if weekPairs:
            for w in range(1,timePeriod.numWeeks+1,2):
                # last week: no weeks left to pair with
                if w == timePeriod.numWeeks:
                    weeksProcs = [proc for proc in sameWeek if proc[iWeek]==w]
                    weeksProcs = self.sortProcedures(weeksProcs)
                    self.packBinsForWeek(w-1,weeksProcs,restrictWeeks,False)
                # pair week's procedures with the following week's and schedule over two week span
                else:
                    weeksProcs = [proc for proc in sameWeek if proc[iWeek]==w or proc[iWeek]==w+1]
                    weeksProcs.sort(lambda x,y:cmp(x[iProcTime],y[iProcTime]))
                    self.packBinsForWeek(w-1,weeksProcs,restrictWeeks,True)                   
        # SAME WEEK procedures: one week spans
        else:
            for w in range(1,timePeriod.numWeeks+1):
                weeksProcs = [proc for proc in sameWeek if proc[iWeek]==w]
                weeksProcs = self.sortProcedures(weeksProcs)                 
                self.packBinsForWeek(w-1,weeksProcs,restrictWeeks,False)
        
        # SAME DAY procedures: two day span (M,T/W,R/F)
        if dayPairs:
            for d in range(1,timePeriod.numDays+1):
                # Wednesday/Friday: do not have to be handled, because they are absorbed into Tuesdays/Thursdays
                if (d%5 == 3) or (d%5 == 0):
                    continue
                # Monday: should not be paired
                elif (d%5 == 1):
                    daysSameDays = [proc for proc in sameDay if proc[iDay]==d]
                    daysSameDays = self.sortProcedures(daysSameDays)                       
                    self.packBinsForDay(d-1,daysSameDays,restrictDays,False)
                # Tuesday/Thursday: should be paired
                else:
                    twoDaysProcs = [proc for proc in sameDay if proc[iDay]==d or proc[iDay]==d+1]
                    twoDaysProcs = self.sortProcedures(twoDaysProcs)                    
                    self.packBinsForDay(d-1,twoDaysProcs,restrictDays,True)
        # SAME DAY procedures: one day span 
        else:
            for d in range(1,timePeriod.numDays+1):
                daysSameDays = [proc for proc in sameDay if proc[iDay]==d]
                daysSameDays = self.sortProcedures(daysSameDays)                  
                self.packBinsForDay(d-1,daysSameDays,restrictDays,False)


        # EMERGENCY procedures: day by day, one day span
        for d in range(1,timePeriod.numDays+1):
            daysEmergencies = [proc for proc in emergencies if proc[iDay]==d]
            daysEmergencies = self.sortProcedures(daysEmergencies)                
            self.packBinsForDay(d-1,daysEmergencies,restrictEmergencies,False)


    def sortProcedures(self,procedures):
        procs = copy.deepcopy(procedures)
        if priority == 'shortest':
            procs.sort(lambda x,y: cmp(x[iProcTime],y[iProcTime]))
        elif priority == 'longest':
            procs.sort(lambda x,y: cmp(x[iProcTime],y[iProcTime]),reverse=True)
        elif priority == 'HBConstraints':
            procs.sort(lambda x,y: cmp(x[iPostTime],y[iPostTime]),reverse=True) 
        return procs

    ######################################## SUMMARY STAT ########################################
    ########################################## METHODS ###########################################

##    def getUtilizationStatistics(self):
##        '''
##        '''
##        CathRooms = {i:[] for i in xrange(numCathRooms)}
##        EPRooms = {i:[] for i in xrange(numEPRooms)}
##        daysUtil = [[copy.deepcopy(CathRooms),copy.deepcopy(EPRooms)] for i in xrange(len(self.dayBins))]
##
##        for day in xrange(len(self.dayBins)):
##            daysBins = self.dayBins[day]
##            cathRoom = daysBins[0]
##            epRoom = daysBins[1]
##            
##            for c in range(numCathRooms):
##                totalMin = self.sumProcTimes(cathRoom[c])
##                util = totalMin / totalTimeRoom
##                daysUtil[day][0][c] = util
##            for e in range(numEPRooms):
##                totalMin = self.sumProcTimes(epRoom[e])
##                util = totalMin / totalTimeRoom
##                daysUtil[day][1][e] = util
##
##        avgDays = self.getAverageUtilizationByDay(daysUtil)
##        avgWeeks = self.getAverageUtilizationByWeek(avgDays)
##
##        avgsCath = [x[0] for x in avgDays]
##        avgsEP = [x[1] for x in avgDays]
##
##        cathAverage = sum(avgsCath)/len(self.dayBins)
##        epAverage = sum(avgsEP)/len(self.dayBins)
##                
##        return (cathAverage,epAverage,avgDays,avgWeeks,daysUtil)
##
##    def getAverageUtilizationByDay(self,daysUtil):
##        '''
##        '''
##
##        daysUtilCopy = copy.deepcopy(daysUtil)
##        daysAverageUtil = [[] for i in xrange(len(self.dayBins))]
##    
##        for d in xrange(len(self.dayBins)):
##            cathDayTotal = 0
##            epDayTotal = 0
##            for c in xrange(self.numCathRooms):
##                cathDayTotal += daysUtilCopy[d][0][c]
##            for e in xrange(self.numEPRooms):
##                epDayTotal += daysUtilCopy[d][1][e]
##            daysAverageUtil[d].append(cathDayTotal/self.numCathRooms)
##            daysAverageUtil[d].append(epDayTotal/self.numEPRooms)
##
##        return daysAverageUtil
##                
##            
##
##    def getAverageUtilizationByWeek(self,avgDays):
##        '''
##        '''
##
##        avgDaysCopy = copy.deepcopy(avgDays)
##        weeksUtil = [[avgDaysCopy[i],avgDaysCopy[i+1],avgDaysCopy[i+2],avgDaysCopy[i+3],avgDaysCopy[i+4]] for i in xrange(0,len(self.dayBins)-4,5)]
##        weeksAverageUtil = [[] for i in xrange(len(self.weekBins))]
##
##        for w in xrange(len(self.weekBins)):
##            cathWeekTotal = 0
##            epWeekTotal = 0
##            week = weeksUtil[w]
##            for d in xrange(5):
##                cathWeekTotal += week[d][0]
##                epWeekTotal += week[d][1]
##            weeksAverageUtil[w].append(cathWeekTotal/5)
##            weeksAverageUtil[w].append(epWeekTotal/5)
##            
##        return weeksAverageUtil
##        
##
    def getOverflowWeeksAndProcs(self):
        '''
        '''

        overflowWeeks = 0
        for week in self.weekBins:
            if len(week[0][2]) > 0:
                overflowWeeks += 1
                        
        overflowProcs = 0
        for day in self.dayBins:
            overflowProcs += len(day[2])

        return (overflowWeeks,overflowProcs)

    def getProcsByMinuteVolume(self,allProcs):
        '''
        '''
        emergencies = [x for x in allProcs if x[iSchedHorizon]==1.0]
        sameDay = [x for x in allProcs if x[iSchedHorizon]==2.0]
        sameWeek = [x for x in allProcs if x[iSchedHorizon]==3.0]

        emergFlex = [x for x in emergencies if x[iRoom]==2.0]
        emergInflex = [x for x in emergencies if x[iRoom]!=2.0]

        sameDayFlex = [x for x in sameDay if x[iRoom]==2.0]
        sameDayInflex = [x for x in sameDay if x[iRoom]!=2.0]

        sameWeekFlex = [x for x in sameWeek if x[iRoom]==2.0]
        sameWeekInflex = [x for x in sameWeek if x[iRoom]!=2.0]

        return [self.sumProcTimes(x) for x in [emergFlex,emergInflex,sameDayFlex,sameDayInflex,sameWeekFlex,sameWeekInflex]]



    ######################################## HELPER FUNCTIONS ########################################
    ######################################### (BIN PACKING) ##########################################

    def sumProcTimes(self,dataList):
        '''
        Input: dataList (list of procedure data)
        
        Returns: the sum of the procedure times in dataList
        '''
        timeDataOnly = [dataList[i][iProcTime] for i in range(len(dataList))]
        
        return sum(timeDataOnly)


    def eliminateRoomsByTimeLimit(self,initialDomain,procedure):
        '''
        '''
        duration = procedure[iProcTime]
        domain = copy.deepcopy(initialDomain)
        toRemove = set()
        for room in domain:
            roomTime = self.sumProcTimes(self.bins[0][room])
            if roomTime > closeCap or roomTime+procedure[iProcTime] > totalTimeRoom:
                toRemove.add(room)
        domain -= toRemove
        return domain

    def updateHoldingBays(self,procedure,day,roomBooked):
        '''
        '''
        # add counters to holding bay
        procStartTime = self.labStartTime + (self.sumProcTimes(roomBooked)-procedure[iProcTime])/60.0
        preHoldingStart = procStartTime - procedure[iPreTime]
        postHoldingStart = procStartTime + procedure[iProcTime]/60.0
        postHoldingEnd = postHoldingStart + procedure[iPostTime]

        # multipliers to round up/down to nearest resolution
        fraction = resolution/60.0
        multiple = 60.0/resolution
        preHoldingStartRound = fraction*math.floor(multiple*preHoldingStart)
        preHoldingEndRound = fraction*math.ceil(multiple*procStartTime)
        postHoldingStartRound = fraction*math.floor(multiple*postHoldingStart)
        postHoldingEndRound = fraction*math.ceil(multiple*postHoldingEnd)

        numPreSlots = (preHoldingEndRound-preHoldingStartRound)/fraction
        numPostSlots = (postHoldingEndRound-postHoldingStartRound)/fraction

        for i in range(int(numPreSlots)):
            self.bins[2][(day,preHoldingStartRound+(i*fraction))] += 1
        
        #The if statement is meant to prevent Dict Key errors when a patient's recovery time 
        #is so long as to exceed the number of available holdingBay slots.
        #However it doesn't quite work and instead I've increased the number of holding bay
        #slots to prevent this error
        #if postHoldingStartRound+(int(numPostSlots)*fraction) <= int(HBCloseTime*multiple):
        for j in range(int(numPostSlots)):
            self.bins[2][(day,postHoldingStartRound+(j*fraction))] += 1
        #else:
        #  for j in range(int(HBCloseTime*multiple)):
        #      holdingBays[postHoldingStartRound+(j*fraction)] += 1

    def updateOverflowStats(self,procOverflow,dayOrWeek,day=True):
        if procOverflow[iLab] == cathID:
            self.overflowCath += 1
        elif procOverflow[iLab] == epID:
            self.overflowEP += 1

        if (day and dayOrWeek not in self.overflowDays):
            self.overflowDays.append(dayOrWeek)
        elif (not day and dayOrWeek not in self.overflowWeeks):
            self.overflowWeeks.append(dayOrWeek)

    def updateProcsPlacedStats(self,procedure):
        self.procsPlaced += 1
        self.procsPlacedData.append(procedure)

    def updateCrossoverStats(self,procedure,placedLabID):
        originalLab = procedure[iLab]
        if originalLab != placedLabID:
            self.crossOverProcs += 1
            if originalLab == cathID:
                self.cathToEP += 1
            else:
                self.epToCath += 1
        
        

    ##################################### DAY BY DAY PACKING #####################################
    ################################### EMERGENCIES/SAME DAYS ####################################

    def packBinsForDay(self,day,daysProcedures,restricted,paired):
        '''
        Schedules procedures during a given day, if possible. Keeps track of overflow procedures
        (that couldn't be scheduled in that day).
        
        Input: day (integer day of time period to be scheduled, indexed from 0)
                daysProcedures (a list of procedure data for a given day)
        Returns: none
        '''

        procs = daysProcedures[:]
        for proc in procs:
            self.tryPlaceProcInLabDay(proc,day,restricted,paired)

    def tryPlaceProcInLabDay(self,procedure,day,restricted,paired):
        '''
        Tries to place a procedure in a given room, if there is time for it in the schedule.
        Input: procedure (list of one procedure's data to be placed)
                lab (string name of lab to be scheduled into)
                day (integer day of time period, indexed from 0)
        Returns: True if placed, False otherwise
        '''

        procDomain = set()

        ### STEP 1: get procedure information ###
        originalLab = procedure[iLab]
        otherLab = cathID if originalLab==epID else epID
        if not restricted:
            originalLabRooms = self.numCathRooms if originalLab==cathID  else self.numEPRooms
            otherLabRooms = self.numCathRooms if originalLab==epID else self.numEPRooms
        else:
            originalLabRooms = self.numRestrictedCath if originalLab==cathID  else self.numRestrictedEP
            otherLabRooms = self.numRestrictedCath if originalLab==epID else self.numRestrictedEP
        flex = True if procedure[iRoom]==2.0 else False
        duration = procedure[iProcTime]

        ### STEP 2: establish initial domain (room choices) ###
        # initial domain is the original lab's rooms for all crossover policies
        for r in xrange(originalLabRooms):
            procDomain.add((day,originalLab,r))
            procDomain.add((day+1,originalLab,r)) if paired else None
        # add other lab's rooms if all procedures can be flexed
        if crossoverType == 'AllFlex':
            for o in xrange(otherLabRooms):
                procDomain.add((day,otherLab,o))
                procDomain.add((day+1,otherLab,o)) if paired else None

        ### STEP 3: constrain domain further ###
        # eliminate rooms that are over the room time limit
        procDomain = self.eliminateRoomsByTimeLimit(procDomain,procedure)

        ### STEP 4: schedule procedure or push to overflow based on domain ###
        # check to see if all room choices have been eliminated: try adding to other lab if possible
        if len(procDomain) == 0 and crossoverType == 'LabPreference' and flex:
            for o in xrange(otherLabRooms):
                procDomain.add((day,otherLab,o))
                procDomain.add((day+1,otherLab,o)) if paired else None
            procDomain = self.eliminateRoomsByTimeLimit(procDomain,procedure)
        # push to overflow: if there are still no room choices
        if len(procDomain) == 0:
            self.bins[1][day].append(procedure)
            self.updateOverflowStats(procedure,day,day=True)
        # schedule procedure: add the procedure to the room with the shortest amount of time already scheduled
        else:
            procDomainList = list(procDomain)
            ascending = sorted(procDomainList, key=lambda x:self.sumProcTimes(self.bins[0][x]))
            toBeBooked = ascending.pop(0)
            roomToBeBooked = self.bins[0][toBeBooked]
            roomToBeBooked.append(procedure)
            self.updateProcsPlacedStats(procedure)
            self.updateCrossoverStats(procedure,toBeBooked[1])
            self.updateHoldingBays(procedure,toBeBooked[0],roomToBeBooked)
                                

    ##################################### WEEK BY WEEK PACKING #####################################
    ################################### SAME WEEK PROCEDURES ONLY ##################################

    def packBinsForWeek(self,week,weeksProcedures,restricted,paired):
        '''
        Schedules procedures during a given week, if possible. Keeps track of overflow procedures
        (that couldn't be scheduled in that week).
        
        Input: week (integer week of time period to be scheduled, indexed from 0)
                weeksProcedures (a list of procedure data for a given week)
                restricted (a boolean value, denoting whether or not to restrict the scheduling to certain Cath/EP rooms
        Returns: none
        '''
        
        procs = weeksProcedures[:]
        for proc in procs:
            self.tryPlaceProcInLabWeek(proc,week,restricted,paired)


    def tryPlaceProcInLabWeek(self,procedure,week,restricted,paired):
        '''
        Tries to place a procedure in a given room, if there is time for it in the week's schedule.
        Input: procedure (list of one procedure's data to be placed)
                lab (string name of lab to be scheduled into)
                week (integer week of time period, indexed from 0)
        Returns: True if placed, False otherwise
        '''

        procDomain = set()

        ### STEP 1: get procedure information ###
        originalLab = procedure[iLab]
        otherLab = cathID if originalLab==epID else epID
        if not restricted:
            originalLabRooms = self.numCathRooms if originalLab==cathID  else self.numEPRooms
            otherLabRooms = self.numCathRooms if originalLab==epID else self.numEPRooms
        else:
            originalLabRooms = self.numRestrictedCath if originalLab==cathID  else self.numRestrictedEP
            otherLabRooms = self.numRestrictedCath if originalLab==epID else self.numRestrictedEP
        flex = True if procedure[iRoom]==2.0 else False
        duration = procedure[iProcTime]
        weekStart = week*5

        ### STEP 2: establish initial domain (room choices) ###
        # initial domain is the original lab's rooms for all crossover policies
        for day in xrange(weekStart,weekStart+5):
            for r in xrange(originalLabRooms):
                procDomain.add((day,originalLab,r))
                procDomain.add((day+5,originalLab,r)) if paired else None
        # add other lab's rooms if all procedures can be flexed
        if crossoverType == 'AllFlex':
            for day in xrange(weekStart,weekStart+5):
                for o in xrange(otherLabRooms):
                    procDomain.add((day,otherLab,o))
                    procDomain.add((day+5,otherLab,o)) if paired else None

        ### STEP 3: constrain domain further ###
        # eliminate rooms that are over the room time limit
        procDomain = self.eliminateRoomsByTimeLimit(procDomain,procedure)

        ### STEP 4: schedule procedure or push to overflow based on domain ###
        # check to see if all room choices have been eliminated: try adding to other lab if possible
        if len(procDomain) == 0 and crossoverType == 'LabPreference' and flex:
            for day in xrange(weekStart,weekStart+5):
                for o in xrange(otherLabRooms):
                    procDomain.add((day,otherLab,o))
                    procDomain.add((day+5,otherLab,o)) if paired else None
            procDomain = self.eliminateRoomsByTimeLimit(procDomain,procedure)
        # push to overflow: if there are still no room choices
        if len(procDomain) == 0:
            self.bins[1][weekStart].append(procedure)
            self.updateOverflowStats(procedure,week,day=False)
        # schedule procedure: add the procedure to the room with the shortest amount of time already scheduled
        else:
            procDomainList = list(procDomain)
            ascending = sorted(procDomainList, key=lambda x:self.sumProcTimes(self.bins[0][x]))
            toBeBooked = ascending.pop(0)
            roomToBeBooked = self.bins[0][toBeBooked]
            roomToBeBooked.append(procedure)
            self.updateProcsPlacedStats(procedure)
            self.updateCrossoverStats(procedure,toBeBooked[1])
            self.updateHoldingBays(procedure,toBeBooked[0],roomToBeBooked)
                        


######################################################################################################
######################################################################################################
##################################### READING/PROCESSING METHODS #####################################
######################################################################################################
######################################################################################################    

def readData(fileName):
    '''
    Input: fileName (string name of the file you want to process procedural data from

    Returns: a list of lists, each one being one procedure's information stored as floats
    '''
    procedures = []
    with open(fileName, 'rU') as f:
        reader = csv.reader(f)
        for row in reader:
            row = [float(i) for i in row[:numEntries+1]]
            procedures.append(row)
    
    return procedures


def cleanProcTimes(allProcs):
    '''
    Input: allProcs (list of all procedures as processed from csv)
    
    Returns: list of all procedures modified so that no procedure is
                of length zero, and procedures of length greater than
                totalTimeCath are truncated
    '''
    newProcs = allProcs[:]

    for i in range (len(newProcs)):
        procTime = newProcs[i][iProcTime]
        procTime += turnover
        if procTime > totalTimeRoom:
          procTime = totalTimeRoom
        newProcs[i][iProcTime] = procTime
    return newProcs


def getOptimizedTimeOnly(timePeriod):
    '''
    Based on a list of optimized scheduling, filter out irrelevant information and only
    include the procedure times.
    Input: optimized (a list, with each element corresponding to 1 day of scheduled procedures.
                Each element itself is a list, consisting of a dictionary with Cath rooms
                and their assigned procedures, a dictionary with EP rooms and their
                assigned procedures, and a list of procedures that went over capacity.
                This should be the output of packBins(procedures)
    Returns: a copy of the same input list, but only including procedure times
    '''
    days = timePeriod.numDays
    optimized = timePeriod.bins
    optimizedCopy = copy.deepcopy(optimized)
    rooms = optimizedCopy[0]
    overflow = optimizedCopy[1]

    for d in xrange(days):
        for c in xrange(numCathRooms):
            daysProcs = rooms[(d,cathID,c)]
            #print "Cath procs: "+str(day)
            timesOnly = [round(x[iProcTime],2) for x in daysProcs]
            rooms[(d,cathID,c)] = timesOnly
        for e in xrange(numEPRooms):
            daysProcs = rooms[(d,epID,e)]
            #print "EP procs: "+str(day)
            timesOnly = [round(x[iProcTime],2)  for x in daysProcs]
            rooms[(d,epID,e)] = timesOnly
        daysOverflow = overflow[d][:]
        overflow[d] = [round(x[iProcTime],2) for x in daysOverflow]
    return optimizedCopy

def getOptimizedTimeAndIDOnly(timePeriod):
    '''
    Based on a list of optimized scheduling, filter out irrelevant information and only
    include the procedure times and procedure ID.
    Input: optimized (a list, with each element corresponding to 1 day of scheduled procedures.
                Each element itself is a list, consisting of a dictionary with Cath rooms
                and their assigned procedures, a dictionary with EP rooms and their
                assigned procedures, and a list of procedures that went over capacity.
                This should be the output of packBins(procedures)
    Returns: a copy of the same input list, but only including procedure times and ID
    '''

    days = timePeriod.numDays
    optimized = timePeriod.bins
    optimizedCopy = copy.deepcopy(optimized)
    rooms = optimizedCopy[0]
    overflow = optimizedCopy[1]

    for d in xrange(days):
        for c in xrange(numCathRooms):
            daysProcs = rooms[(d,cathID,c)]
            #print "Cath procs: "+str(day)
            timesOnly = [(x[ID],round(x[iProcTime],2) ) for x in daysProcs]
            rooms[(d,cathID,c)] = timesOnly
        for e in xrange(numEPRooms):
            daysProcs = rooms[(d,epID,e)]
            #print "EP procs: "+str(day)
            timesOnly = [(x[ID],round(x[iProcTime],2) ) for x in daysProcs]
            rooms[(d,epID,e)] = timesOnly
        daysOverflow = overflow[d][:]
        overflow[d] = [(x[ID],round(x[iProcTime],2) ) for x in daysOverflow]
    return optimizedCopy


def cleanResults(newOptimized,timePeriod):
    '''
    Input: optimizedTimeOnly (a list of the optimized procedure times. Should be output from
            getOptimizedTimeOnly(optimized)
    Returns: a copy of the same input list, but with all days equalized in terms of number of
                procedures listed per room day and overflow procedures per day
    '''
    days = timePeriod.numDays
    optimizedCopy = newOptimized[:]
    rooms = optimizedCopy[0]
    overflow = optimizedCopy[1]
    
    # get the OVERALL maximum number of procedures in Cath/EP/Overflow for the time period
    maxNumProcsCathRoom = 0
    maxNumProcsEPRoom = 0
    maxNumOverflow = 0
    for d in xrange(days):
        for c in xrange(numCathRooms):
            room = rooms[(d,cathID,c)]
            maxNumProcsCathRoom = len(room) if len(room)>maxNumProcsCathRoom else maxNumProcsCathRoom
        for e in xrange(numEPRooms):
            room = rooms[(d,epID,e)]
            maxNumProcsEPRoom = len(room) if len(room)>maxNumProcsEPRoom else maxNumProcsEPRoom
            
        maxNumOverflow = len(overflow[d]) if len(overflow[d])>maxNumOverflow else maxNumOverflow
 
    # add elements to each room day if necessary, so each room day has the same number of elements
    for d in xrange(days):        
        for c in xrange(numCathRooms):
            room = rooms[(d,cathID,c)]
            numDiffProcs = maxNumProcsCathRoom - len(room)
            room += numDiffProcs*[0.00]
        for e in xrange(numEPRooms):
            room = rooms[(d,epID,e)]
            numDiffProcs = maxNumProcsEPRoom - len(room)
            room += numDiffProcs*[0.00]
        overflowList = overflow[d]
        numDiffProcs = maxNumOverflow - len(overflowList)
        overflowList += numDiffProcs*[0.00]
    return optimizedCopy


def saveSchedulingResults(cleanOptimizedTimeOnly,timePeriod,workbook):

    out = open(workbook,'wb')
    writer = csv.writer(out)

    maxNumCathProcs = len(cleanOptimizedTimeOnly[0][(0,cathID,0)])
    maxNumEPProcs = len(cleanOptimizedTimeOnly[0][(0,epID,0)])

    numCathColumns = numCathRooms*maxNumCathProcs
    numEPColumns = numEPRooms*maxNumEPProcs
    numOverflowColumns = len(cleanOptimizedTimeOnly[1][0])

    days = timePeriod.numDays

    columns = ['Day']
    for c in xrange(numCathRooms):
        for i in xrange(maxNumCathProcs):
            columns.append('Cath Room '+str(c+1)+' Proc '+str(i+1))
    for e in xrange(numEPRooms):
        for j in xrange(maxNumEPProcs):
            columns.append('EP Room '+str(e+1)+' Proc '+str(j+1))
    for o in xrange(numOverflowColumns):
        columns.append('Overflow Proc '+str(o+1))
    writer.writerow(columns)

    data = []
    for d in xrange(1,days+1):
        day = [str(d)]
        # write room-procedure/overflow-procedure information
        for cath in xrange(numCathRooms):
            for proc in xrange(1,maxNumCathProcs+1):
                day.append(str(cleanOptimizedTimeOnly[0][(d-1,cathID,cath)][proc-1]))
        for ep in xrange(numEPRooms):
            for proc in xrange(1,maxNumEPProcs+1):
                day.append(str(cleanOptimizedTimeOnly[0][(d-1,epID,ep)][proc-1]))
        for o in xrange(1,numOverflowColumns+1):
            day.append(str(cleanOptimizedTimeOnly[1][d-1][o-1]))
        data.append(day)
    writer.writerows(data)


def saveHoldingBayResults(timePeriod,workbook):

    out = open(workbook,'wb')
    writer = csv.writer(out)

    multiple = 60.0/resolution
    times = [i/multiple for i in xrange(int(HBCloseTime*multiple))]
    columns = ["Day"]
    for time in times:
        hours = math.floor(time)
        minutes = (time - math.floor(time))*60
        columns.append(str(int(hours))+":"+str(int(minutes)))
    writer.writerow(columns)
    
    data = []
    for d in xrange(timePeriod.numDays):
        holdingBays = timePeriod.bins[2]
        day = [holdingBays[(d,time)] for time in times]
        day.insert(0,str(d+1))
        data.append(day)

    writer.writerows(data)

def printOutputStatistics(timePeriod):

    print "*********PARAMETERS*********"
    print "Cath rooms: "+str(numCathRooms)
    print "EP rooms: "+str(numEPRooms)
    print "Cath rooms used for non-emergencies: "+str(numRestrictedCath)
    print "EP rooms used for non-emergencies: "+str(numRestrictedEP)
    print "Crossover policy: "+str(crossoverType)
    print "Pair weeks for scheduling? "+str(weekPairs)
    print "Pair days for scheduling? "+str(dayPairs)
    print "Schedule all procedures on same day as historically? "+str(sameDaysOnly)
    print "Placement priority: "+str(priority)
    print "Post procedure determination random? "+str(postProcRandom)+"\n"

    print "*********PROCEDURE DATA*********"
    print "Total procedures: "+str(timePeriod.numTotalProcs)
    print "Same days: "+str(timePeriod.numSameDays)
    print "Same weeks: "+str(timePeriod.numSameWeeks)
    print "Emergencies: "+str(timePeriod.numEmergencies)
    minutes = timePeriod.getProcsByMinuteVolume(procedures)
    for x in xrange(6):
        minutes[x] = round(minutes[x],2)
    print "\tBREAKDOWN BY MINUTES"
    print "\tSame week flex: "+str(minutes[4])+" minutes"
    print "\tSame week inflex: "+str(minutes[5])+" minutes"
    print "\tSame day flex: "+str(minutes[2])+" minutes"
    print "\tSame day inflex: "+str(minutes[3])+" minutes"
    print "\tEmergency flex: "+str(minutes[0])+" minutes"
    print "\tEmergency inflex: "+str(minutes[1])+" minutes"+"\n"

    
    print "*********OVERFLOW STATS*********"
    print "Total of "+str(timePeriod.procsPlaced)+" procedures placed"
    print "Total procedures that went to overflow: "+str(timePeriod.overflowCath+timePeriod.overflowEP)
    print "\tCath overflow: "+str(timePeriod.overflowCath)
    print "\tEP overflow: "+str(timePeriod.overflowEP)
##    print "Overflow weeks: "+str(timePeriod.getOverflowWeeksAndProcs()[0])
##    print "Overflow procedures: "+str(timePeriod.getOverflowWeeksAndProcs()[1])
    print "Same week procedures overflow during weeks (0 index): "+str(sorted(timePeriod.overflowWeeks))
    print "Same day/emergencies overflow during days (0 index): "+str(sorted(timePeriod.overflowDays))
    minutesPlaced = timePeriod.getProcsByMinuteVolume(timePeriod.procsPlacedData)
    print "\tBREAKDOWN BY MINUTES PLACED"
    modifiedMinutes = [0]*6
    for x in xrange(6):
        minutesPlaced[x] = round(minutesPlaced[x],2)
        modifiedMinutes[x] = 100 if minutes[x]==0 else minutes[x]
    print "\tSame week flex: "+str(minutesPlaced[4])+" out of "+str(minutes[4])+" minutes placed ("+str(round((minutesPlaced[4]/(modifiedMinutes[4])*100),2))+"%)"
    print "\tSame week inflex: "+str(minutesPlaced[5])+" out of "+str(minutes[5])+" minutes placed ("+str(round((minutesPlaced[5]/(modifiedMinutes[5])*100),2))+"%)"
    print "\tSame day flex: "+str(minutesPlaced[2])+" out of "+str(minutes[2])+" minutes placed ("+str(round((minutesPlaced[2]/(modifiedMinutes[2])*100),2))+"%)"
    print "\tSame day inflex: "+str(minutesPlaced[3])+" out of "+str(minutes[3])+" minutes placed ("+str(round((minutesPlaced[3]/(modifiedMinutes[3])*100),2))+"%)"
    print "\tEmergency flex: "+str(minutesPlaced[0])+" out of "+str(minutes[0])+" minutes placed ("+str(round((minutesPlaced[0]/(modifiedMinutes[0])*100),2))+"%)"
    print "\tEmergency inflex: "+str(minutesPlaced[1])+" out of "+str(minutes[1])+" minutes placed ("+str(round((minutesPlaced[1]/(modifiedMinutes[1])*100),2))+"%)"+"\n"
    
    print "*********CROSSOVER STATS*********"
    print "Total number of crossover procedures: "+str(timePeriod.crossOverProcs)
    print "Total number of Cath procedures in EP: "+str(timePeriod.cathToEP)
    print "Total number of EP procedures in Cath: "+str(timePeriod.epToCath)+"\n"
##    
##    print "*********UTILIZATION STATS*********"
##    cath, ep, avgUtilDay, avgUtilWeek, util = timePeriod.getUtilizationStatistics()
##    print "Average utilization in Cath over time period: "+str(cath)
##    print "Average utilization in EP over time period: "+str(ep)
##    print "type: 'avgUtilDay[day]' to view average utilization in Cath and EP on a given day (indexed from 0)"
##    print "type: 'avgUtilWeek[week]' to view average utilization in Cath and EP during a given week (indexed from 0)"
##    print "type: 'util[day]' to view the full utilization breakdown by lab and room on a given day (indexed from 0)"
##        

######################################################################################################
######################################################################################################
#################################### CONFIGURING/RUNNING THE SCRIPT ##################################
######################################################################################################
###################################################################################################### 
        

if __name__ == "__main__":


    ############# VERIFY FOLLOWING VALUES BEFORE RUNNING ##############

    ###### information regarding the scheduling parameters ######
    
    totalTimeRoom = 10.58*60    # total time available in a room per day (min)
    closeCap = 10*60            # time cap for closing a room (min)
    turnover = 0                # estimated time for room turnover (min)
    labStartTime = 8          # time of morning that the lab starts operating (8.0 = 8:00 AM, 8.5 = 8:30 AM, etc)
    
    numCathRooms = 5            # number of Cath rooms available per day
    numEPRooms = 3              # number of EP rooms available per day
    
    numRestrictedCath = 5       # default to no reserved rooms for emergencies
    numRestrictedEP = 3         # default to no reserved rooms for emergencies
    restrictWeeks = True        # whether or not to restrict the same week procedures to the number of restricted rooms
    restrictDays = True         # whether or not to restrict the same day procedures to the number of restricted rooms
    restrictEmergencies = False # whether or not to restrict the emergency procedures to the number of restricted rooms


    ###### information regarding constraint policies ######
    
    # UNCOMMENT the type of crossover policy you want to implement
    crossoverType = "LabPreference"    #Allows crossovers as coded in the data, but attempts to place procedures in their own lab
    #crossoverType = "NoCrossovers"
    #crossoverType = "AllFlex"

    # UNCOMMENT the week pairing policy you want to implement
    weekPairs = True            # will schedule same week procedures across a two week span
    #weekPairs = False           # will schedule same week procedures during their original one week span
    dayPairs = True
    #dayPairs = False


    # UNCOMMENT the same day/same week policy you want to implement
    #sameDaysOnly = True         # will schedule all procedures on the day they were historically scheduled
    sameDaysOnly = False        # will shedule procedures based on their scheduling horizon


    # Information for holding bays
    #UNCOMMENT the post procedure time policy you want to implement
    #postProcRandom = True       # will draw the post procedure time from a random distribution with a specified mean and std deviation
    postProcRandom = False      # will use the post procedure time specified in the input data
    #desiredMean = 3.0           # in hours
    #desiredStDev= 0.25          # in hours
    #Set the number of hours in the day after which the holding bays should close:
    HBCloseTime = 30            #Default is 24

    # SPECIFY the resolution for holding bay times
    resolution = 15.0           # in minutes

    # UNCOMMENT the placement priority you want to implement
    #priority = 'shortest'
    priority = 'longest'
    #priority = 'none'
    #priority = 'HBConstraints'


    ###### information regarding the order of information in the data sheet ######
    
    numEntries = 10             # number of columns in data sheet
    iDay = 0                    # index: Day of period
    iWeek = 1                   # index: Week of period
    iLab = 2                    # index: Lab key (Cath,EP)
    iProcTime = 3               # index: Procedure time (minutes)
    iSchedHorizon = 4           # index: Scheduling horizon key
    iRoom = 5                   # index: Room constraint (Cath only, EP only, either)
    iProcType = 8               # index: Procedure type key
    iProvider = 9               # index: Provider key
    iPreTime = 6                # index: The amount of pre-procedure holding time needed (minutes)
    iPostTime = 7               # index: The amount of post-procedure holding time needed (minutes)
    ID = numEntries             # index: The procedure ID

    cathID = 0.0
    epID = 1.0
    
    daysInPeriod = 125          # integer: Number of days in period
    
    roomConstraint = {0.0:'Cath', 1.0:'EP', 2.0:'either'}
                                # room constraint key used in data file


    ###### information regarding the name/location of the data file ######

    # UNCOMMENT the working directory, or add a new one
    os.chdir("/Users/nicseo/Desktop/MIT/Junior/Fall/UROP/Scheduling Optimization/Script")
    #os.chdir("/Users/dscheink/Documents/MIT-MGH/EP_Cath/Git/mghSchedulingModel/")
    
    # UNCOMMENT the data set to analyze, or add a new one
    fileName= 'InputData/CathFlatEPFlat.csv'
    #fileName= 'InputData/CathFlatEPGrow1.csv'
    #fileName= 'InputData/CathFlatEPGrow2.csv'
    #fileName= 'InputData/CathDrop1EPFlat.csv'
    #fileName= 'InputData/CathDrop1EPGrow1.csv'
    #fileName= 'InputData/CathDrop1EPGrow2.csv'
    #fileName= 'InputData/CathDrop2EPFlat.csv'
    #fileName= 'InputData/CathDrop2EPGrow1.csv'
    #fileName= 'InputData/CathDrop2EPGrow2.csv'
    
    #fileName = 'InputData/TestInput.csv'


    ###### information regarding the name/location of the output data ######
    ########## which must be created before running this script ############
    
    # please name the workbook to save the primary output to
    mainWorkbook = "scheduleV2.csv"
    detailedWorkbook = "detailedScheduleV2.csv"

    # please name the workbook to save the holding bay output to
    holdingBayWorkbook = "holdingBaysV2.csv"    


    ############# RUNNING OF THE SCRIPT: not necessary to modify #############

    ###### read/process in data ######
    procedures = readData(fileName)
    procedures = cleanProcTimes(procedures)
    
    ###### model time period / pack bins ######
    timePeriod = TimePeriod(daysInPeriod,numCathRooms,numEPRooms,numRestrictedCath,numRestrictedEP,labStartTime)
    timePeriod.packBins(procedures,crossoverType,weekPairs,dayPairs)

    printOutputStatistics(timePeriod)
    
    ###### process results ######
    optimized = copy.deepcopy(timePeriod.bins)
    
    optimizedTimeOnly = getOptimizedTimeOnly(timePeriod)
    cleanedOptimizedTime = cleanResults(optimizedTimeOnly,timePeriod)

    optimizedTimeAndIDOnly = getOptimizedTimeAndIDOnly(timePeriod)
    cleanedOptimizedTimeID = cleanResults(optimizedTimeAndIDOnly,timePeriod)

    ###### save results ######
    saveHoldingBayResults(timePeriod,holdingBayWorkbook)
    saveSchedulingResults(cleanedOptimizedTime,timePeriod,mainWorkbook)
    saveSchedulingResults(cleanedOptimizedTimeID,timePeriod,detailedWorkbook)

