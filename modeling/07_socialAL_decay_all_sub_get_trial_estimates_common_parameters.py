#!/usr/bin/env python
# coding: utf-8

# # SocialAL Model
# # Trial-wise estimates from common parameters - multiple subjects
# KLS 7.22.21 
# Project info: https://osf.io/b48n2/
# 
# Model modified from :
# Fareri, D. S., Chang, L. J., & Delgado, M. R. (2012). Effects of direct social experience on trust decisions and neural reward circuitry. Frontiers in Neuroscience, 6, 1–17. https://doi.org/10.3389/fnins.2012.00148
# 
# Collins, A. G. E., & Frank, M. J. (2012). How much of reinforcement learning is working memory, not reinforcement learning? A behavioral, computational, and neurogenetic analysis. European Journal of Neuroscience, 35(7), 1024–1035. https://doi.org/10.1111/j.1460-9568.2011.07980.x

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

def get_trial_estimates(params, data):
    sub = params[0]
    a_gain = params[1]
    a_loss = params[2]
    beta = params[3]
    decay = params[4]
    
    # initialize variables
    prob = [0.5, 0.5, 0.5]
    ev = [[9,9,9,9],[9,9,9,9],[9,9,9,9]]
    tletters = ["A", "B", "C"]
    
    trial_data = []
    for trial in range(0, len(data)):
        
        trustee = data['Stim_Sequence'][trial] # get trustee type
        choice = data['Choice'][trial] # get choice made by participant
        response = data['Trustee_Response'][trial] # get response from trustee
        
        # record prob of reciprocation at beginning of trial
        prob_recip = prob[trustee]
        
        # calculate prediction error for the trial
        pe = response - prob[trustee]
        
        # compute the probability of selecting each option for that trustee
        probs = get_action_selection_probs(beta, ev[trustee])
        
        if choice != 0:
            # use the probability of the selection (choice-probability) to update log likelihood
            cprob = probs[choice-1] #print(cprob, isinstance(cprob, float))

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
            
        trial_data.append([sub, trial+1, prob_recip, pe])
        
    return trial_data


# ### Load and clean data

# In[17]:


meanparams = pd.read_csv('../../output/model_parameter_means.csv')
meanlist = meanparams.values.tolist()


# In[18]:


#files = os.listdir('../../data/modeling')
files = meanparams['id']
files.head(5)


# In[21]:


def load_and_clean(file):
    filename = file + '.csv'
    path = os.path.join('../../data/modeling', filename)
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


# In[26]:


trialest = [get_trial_estimates(meanlist[x], load_and_clean(files[x])) for x in range(len(files))]
#trialest


# In[27]:


# add a loop here to save a separate file for each participant
for x in range(len(files)):
    filepath = '../../output/trial_estimates_common/' + files[x] + '.csv'
    #print(filepath)
    sub = pd.DataFrame(trialest[x], columns = ['id', 'trial', 'prob_recip', 'pe'])
    #print(sub.head(5))
    sub.to_csv(path_or_buf = filepath, index = False)


# In[ ]:




