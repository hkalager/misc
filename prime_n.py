#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar  9 13:34:37 2022

@author: arman
"""
from datetime import datetime
t0=datetime.now()
def find_prime(n):
    current_list=[]
    v=2
    
    while(len(current_list)<n):
        divider_set=range(2,v//2+1)
        if len(divider_set)==0:
            current_list.append(v)
        else:
            found_divider=False
            vv=divider_set[0]
            while found_divider==False and vv in divider_set:
                found_divider=(v%vv==0)
                if found_divider==False:
                    try:
                        vv=divider_set[divider_set.index(vv)+1]
                    except:
                        vv=vv+1
                        current_list.append(v)
        v+=1

    return current_list
    
n=50
first_n_prime=find_prime(n)
t1=datetime.now()
dt=t1-t0
dt.microseconds
print('The first '+str(n)+' prime numbers are ...\n')
print(first_n_prime)
print('total run time is '+str(dt.microseconds)+' M seconds')
