# -*- coding: utf-8 -*-
"""
Created on Thu Jul  2 01:20:42 2020

@author: felip
"""
import cv2
#import numpy as np
import os 
#from  matplotlib import pyplot as plt
import glob
import pandas as pd
##seleccionando directiorio en este momento no es interactivo
path =r"C:\Users\felip\Documents\PhD\Douda\experiments joanna\Second approach\Pruebabinomial"
os.chdir(path)
filenames=glob.glob('*.mp4')
for i in filenames:
    ## leyendo videos y seleccionadno ROI
    cap = cv2.VideoCapture("NEGATIVE1-T2M2.mp4")
    videol=int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    first=None
    cnt_old=None
    hu=[]
    hu.append(0)
    area=[]
     # Capture frame-by-frame
    ret, frame = cap.read()  # ret = 1 if the video is captured; frame is the image
    r = cv2.selectROI(frame)
    while (cap.isOpened()):
        ret, frame = cap.read() 
        if ret == False:
            break
        imCrop = frame[int(r[1]):int(r[1]+r[3]), int(r[0]):int(r[0]+r[2])]
        framea=cv2.cvtColor(imCrop, cv2.COLOR_BGR2GRAY)
        blur=cv2.GaussianBlur(framea,(5,5),0)
        ret,otsu=cv2.threshold(blur,0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
        cnt, hrq =cv2.findContours(otsu, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        cv2.drawContours(framea, cnt[0], -1, (0, 0, 0),  3  )
        if cnt_old is not None:
            Hu=cv2.matchShapes(cnt_old, cnt[0], cv2.CONTOURS_MATCH_I3, 0.0)
            cnt_old=cnt[0]
            hu.append(Hu)
        else:
            cnt_old=cnt[0]
        Area=cv2.contourArea(cnt[0])
        area.append(Area)

resultfinal={ 'Hu moments':hu, 'Area':area}
dffinal=pd.DataFrame(resultfinal)
dffinal.to_csv('resultados'+str(i)+'.csv', header=True)

    
