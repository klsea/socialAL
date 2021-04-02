#!/usr/bin/env python
# coding: utf-8

# #SocialAL Model
# 
# #Fit single alpha model to data - multiple subjects
# 
# KLS 2.24.20
# Project info: https://osf.io/b48n2/
# 
# Model modified from : Fareri, D. S., Chang, L. J., & Delgado, M. R. (2012). Effects of direct social experience on trust decisions and neural reward circuitry. Frontiers in Neuroscience, 6, 1–17. https://doi.org/10.3389/fnins.2012.00148

# In[1]:


import sys
print(sys.version)
import numpy as np
import random
import math
import pandas as pd
from scipy.optimize import minimize
import os
from decimal import *


# In[2]:


def update_value(Prob):
    invest = [0,3,6,9]
    retain = [9-x for x in invest] #print ("Retain list is: ", retain)
    shared = [2*x for x in invest] #print ("Shared list is: ", shared)
    EV = [(retain[x] + Prob*shared[x]) for x in range(0,4)]
    return EV

def get_action_selection_prob(beta, EV, choice):
    actionProb = Decimal(np.exp(beta*EV[choice-1])/np.sum([np.exp(beta*x) for x in EV]))
    return actionProb

def get_action_selection_probs(beta, EV):
    actionProbs = [get_action_selection_prob(beta, EV, x) for x in range(1,5)]
    return actionProbs

def get_likelihood_action(params, data):
    beta = params[0]
    
    # initialize variables
    prob = [0.5, 0.5, 0.5]
    ev = [[9,9,9,9],[9,9,9,9],[9,9,9,9]]
    
    totalLLH = 0 
    for trial in range(0, len(data)):
        trustee = data['Stim_Sequence'][trial] # get trustee type
        choice = data['Choice'][trial] # get choice made by participant
        response = data['Trustee_Response'][trial] # get response from trustee
        
        # compute the probability of selecting each option for that trustee
        probs = get_action_selection_probs(beta, ev[trustee])
        
        # use the probability of the selection (choice-probability) to update log likelihood
        if choice != 0:
            cprob = probs[choice-1] #print(cprob, isinstance(cprob, float))
            #print(cprob)

            #add to cumulative log likelihood
            totalLLH += -(math.log(cprob))
        
            # update value - no probability update in baseline b/c no learning
            ev[trustee] = update_value(prob[trustee])
        
    return totalLLH

def model_fit(data):
    
    tries = 10000 #  number of tries to find the best-fit parameter
    lowestLLH = math.inf 
    bestFit = 'NA'
    
    for i in range(tries):
        
        # initialize free parameters with randomly chosen numbers
        beta=random.uniform(0, 1)
        params = [beta]

        # trying different solvers in the minimize call...
        results = minimize(get_likelihood_action, 
                           params, args =(data), bounds = [(1e-10, 20)], 
                           options = {'maxiter': 10000, 'disp': False})
        if (lowestLLH > results['fun'] and results['success']== True):
            lowestLLH = results['fun']
            bestFit = results
            
    return bestFit


# Load and Clean Data

# In[3]:


files = os.listdir('../data/modeling')


# In[4]:


def load_and_clean(file):
    path = os.path.join('../data/modeling', file)
    dt = pd.read_csv(path)
    # recode trial type into numbers for model
    def stims(trial_type):
        if trial_type == "Trustworthy":
            return 0
        elif trial_type == "Neutral":
            return 1
        elif trial_type == "Untrustworthy":
            return 2
    dt['Stim_Sequence'] = dt['trial_type'].apply(stims)
    # rename response_key to choice
    def choices(response_key):
        if response_key == 'None':
            return 0 
        else:
            return response_key  
    dt['Choice'] = dt['response_key'].apply(choices)
    dt['Choice'] = pd.to_numeric(dt['Choice'])
    # calculte the trustee response
    def resp(trial_earnings):
        if trial_earnings >= 12:
            return 1
        else:
            return 0
    dt['Trustee_Response'] = dt['trial_earnings'].apply(resp)
    data = dt[['Stim_Sequence','Choice', 'Trustee_Response']]
    return(data)


# Fit Model

# In[5]:


print(getcontext())
params = [model_fit(load_and_clean(file)) for file in files]


# In[7]:


#params


# In[10]:


params = pd.DataFrame(params)
params['id'] = [file[:-4] for file in files]
#params['alpha'] = [params['x'][y][0] for y in range(len(params))]
params['beta'] = [params['x'][y][0] for y in range(len(params))]
params['-LLH'] = [params['fun'][y] for y in range(len(params))]
params = params[['id', 'beta', '-LLH']]
print(params)   


# In[11]:


# save parameters in text file
params.to_csv(path_or_buf = '../output/baseline_model_params.csv', index = False)

