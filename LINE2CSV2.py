#!/usr/bin/python
# -*-coding:utf-8 -*
# Language: English
# Author: Simon BESSON-GIRARD
# mail: simon@besson-girard.fr
# Description: date with dot ; date and time are not separated

#import numpy as np
#import pandas as pd
#import os.path as op
#import commands as c

def main(file):
	f=open(file, 'r')
	t=f.readline()
	t=f.readline()
	t=f.readline()
	t=f.readline()
	date=[]
	time=[]
	message=[]
	author=[]
	g=open(file[:-4]+'_2.csv','w')
	g.write('datetime\tauthor\tmessage\n')
	while t: 
		s=t.split("\t")
		if len(s)==1: 
		#if len == 1 either date either same time
			if s[0][0:4] in ["lun.","mar.","mer.","jeu.","ven.","sam.","dim."]:
			#to select dates
				date.append(s[0].split(" ")[1][:-2])
			else:
			#continuity and same date and time than last register
				message.append(str(s[0]))
		else:
		#time author message
			time.append(str(s[0]))
			author.append(str(s[1]))
			message.append(str(s[2]))
		if not len(time)==0:
			g.write(date[len(date)-1]+' '+time[len(time)-1]+'\t'+author[len(author)-1]+'\t'+message[len(message)-1]+'\n')
		t=f.readline()
	f.close()
	g.close()
	
	
