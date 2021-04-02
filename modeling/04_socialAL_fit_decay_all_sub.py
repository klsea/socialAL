#!/usr/bin/env python
# coding: utf-8

# # SocialAL Model
# # Fit model to data - multiple subjects
# KLS 10.4.20  
# Project info: https://osf.io/b48n2/
# 
# Model modified from :
# Fareri, D. S., Chang, L. J., & Delgado, M. R. (2012). Effects of direct social experience on trust decisions and neural reward circuitry. Frontiers in Neuroscience, 6, 1–17. https://doi.org/10.3389/fnins.2012.00148

# ### Python Version

# In[1]:


import sys
print(sys.version)  


# ### Load modules

# In[2]:


import numpy as np
import random
import math
import pandas as pd
from scipy.optimize import minimize
import os
from decimal import *
getcontext().prec = 1000 # increases the precision of data representation 


# ### Define functions

# In[3]:


def update_value(Prob):
    invest = [0,3,6,9]
    retain = [9-x for x in invest] #print ("Retain list is: ", retain)
    shared = [2*x for x in invest] #print ("Shared list is: ", shared)
    EV = [(retain[x] + Prob*shared[x]) for x in range(0,4)]
    return EV

def update_prob(recip, Prob, a_gain, a_loss):
    gain = max(recip - Prob, 0)
    loss = min(recip - Prob, 0)
    Prob = Prob + a_gain * gain + a_loss * loss
    return Prob

def get_action_selection_prob(beta, EV, choice):
    actionProb = Decimal(np.exp(beta*EV[choice-1])/np.sum([np.exp(beta*x) for x in EV]))
    return actionProb

def get_action_selection_probs(beta, EV):
    actionProbs = [get_action_selection_prob(beta, EV, x) for x in range(1,5)]
    return actionProbs

def get_likelihood_action(params, data):
    a_gain = params[0]
    a_loss = params[1]
    beta = params[2]
    decay = params[3]
    
    # initialize variables
    prob = [0.5, 0.5, 0.5]
    ev = [[9,9,9,9],[9,9,9,9],[9,9,9,9]]
    tletters = ["A", "B", "C"]
    
    totalLLH = 0 
    for trial in range(0, len(data)):

        trustee = data['Stim_Sequence'][trial] # get trustee type
        choice = data['Choice'][trial] # get choice made by participant
        response = data['Trustee_Response'][trial] # get response from trustee
        
        # compute the probability of selecting each option for that trustee
        probs = get_action_selection_probs(beta, ev[trustee])
        
        if choice != 0:
            # use the probability of the selection (choice-probability) to update log likelihood
            cprob = probs[choice-1] #print(cprob, isinstance(cprob, float))
           
            #add to cumulative log likelihood
            totalLLH += -(math.log(cprob))
            

            # update prob and value for trustee 
            if choice != 1:
                prob[trustee] = update_prob(response, prob[trustee], a_gain, a_loss)         
            ev[trustee] = update_value(prob[trustee])
            
        # decay prob and value for other possible trustees 
        options = [0, 1, 2]
        if choice == 2 or choice == 3 or choice == 4:
            options.pop(trustee)
        for x in options:
            prob[x] = prob[x] + decay * (0.5 - prob[x])
            ev[x] =  update_value(prob[x])
    return totalLLH

def model_fit(data):
    
    tries = 10000 #  number of tries to find the best-fit parameter
    lowestLLH = math.inf 
    bestFit = 'NA'
    
    for i in range(tries):
        
        # initialize free parameters with randomly chosen numbers
        a_gain=random.uniform(0, 1)
        a_loss=random.uniform(0, 1)
        beta=random.uniform(0, 1)
        decay=random.uniform(0,1)
        params = [a_gain, a_loss, beta, decay]

        # trying different solvers in the minimize call...
        results = minimize(get_likelihood_action, 
                           params, args =(data), bounds = [(0, 1), (0, 1), (1e-10, 20), (0, 1)], 
                           options = {'maxiter': 10000, 'disp': False})
        if (lowestLLH > results['fun'] and results['success']== True):
            lowestLLH = results['fun']
            bestFit = results
        if bestFit == 'NA':
            line = ['NA', 'NA', 'NA', 'NA', 'NA']
        else:
            line = np.append(bestFit['x'], bestFit['fun'])
      
    return line


# ### Load and clean data

# In[4]:


files = os.listdir('../data/modeling')


# In[5]:


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


# In[ ]:


print(getcontext())
params = [model_fit(load_and_clean(file)) for file in files]


# In[ ]:


params = pd.DataFrame(params)
params['id'] = [file[:-4] for file in files]
# rename columns
params = params.rename({0:'alpha_gain', 1:'alpha_loss', 2:'beta', 3:'decay', 4:'-LLH', 'id':'id'}, axis='columns') 
cols = params.columns.tolist()
cols = cols[-1:] + cols[:-1]
params = params[cols]
params


# In[ ]:


# save parameters in text file
params.to_csv(path_or_buf = '../output/two_alpha_with_decay_model_params.csv', index = False)


# In[ ]:




