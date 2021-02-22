# -*- coding: utf-8 -*-
"""
Created on Thu Jul  2 01:20:42 2020

@author: felip
"""
import cv2
import numpy as np
import os 
from  matplotlib import pyplot as plt
import glob
import pandas as pd
##seleccionando directiorio en este momento no es interactivo
path =r"C:\Users\felip\Documents\PhD\Douda\experiments joanna\Second approach\analisis  videos completo\controls\enhanced"
os.chdir(path)
filenames1=glob.glob('*.mp4') 
filenames2=glob.glob('*.avi')
filenames=filenames1+filenames2
rs=[]
#ri=0
def analisisvideo( video):
   cap = cv2.VideoCapture(video)
   eccentricity=[]
   circularity=[]
   area=[]
   hu=[]
   v=[]
     # Capture frame-by-frame
   ret, frame = cap.read()  # ret = 1 if the video is captured; frame is the image
   r = cv2.selectROI(frame)
   rs.append(r)
    #r=rs.iloc[ri]
    #ri=ri+1
   while (cap.isOpened()):
       ret, frame = cap.read() 
       if ret == False:
               break
       #the image is converted to grasyscale for BGR color spcae despite the fact that  the video is in 
       # yuv420p color space according to ffmpeg, this is becasue opencv load images of color as BGR as default
       # look https://stackoverflow.com/questions/26059507/how-can-i-know-if-a-image-is-rgb-in-opencv
       imCrop = frame[int(r[1]):int(r[1]+r[3]), int(r[0]):int(r[0]+r[2])]
       framea=cv2.cvtColor(imCrop, cv2.COLOR_BGR2GRAY)
       hsv=cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
       V=hsv[...,2].mean()
       blur=cv2.GaussianBlur(framea,(5,5),0)
       ret,otsu=cv2.threshold(blur,0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
       otsuf=255-otsu
       mom=cv2.moments(otsuf, binaryImage=True )
       #perc1=cv2.copyMakeBorder(otsuf,1,1,1,1, cv2.BORDER_CONSTANT)
       #can=cv2.Canny(perc1,0,254)
       cnt,hierarchy = cv2.findContours(otsuf, 1, 2)
       per=cv2.arcLength(cnt[0],True)
       Area=mom['m00']
       circ=pow(per, 2)/Area
       ecc=(pow(mom['mu20']-mom['mu02'], 2) - 4*pow(mom['mu11'] , 2))/pow(mom['mu20']+mom['mu02'],2)
       area.append(Area)
       eccentricity.append(ecc)
       circularity.append(circ)
       v.append(V)
   resultfinal=pd.DataFrame(list(zip(area, eccentricity, circularity,v)), columns=['Area', 'Eccentricity', 'Circularity', 'Brightness'])
   resultfinal.to_csv('resultados'+str(video)+'.csv', header=True)
   
   
   ## funcion con rois predeterminados

def analisisvideor( video):
   cap = cv2.VideoCapture(video)
   eccentricity=[]
   circularity=[]
   area=[]
   hu=[]
     # Capture frame-by-frame
   ret, frame = cap.read()  # ret = 1 if the video is captured; frame is the image
   ind=filenames.index(video)
   r = rs.loc[ind]
    #r=rs.iloc[ri]
    #ri=ri+1
   while (cap.isOpened()):
       ret, frame = cap.read() 
       if ret == False:
           break
       imCrop = frame[int(r[1]):int(r[1]+r[3]), int(r[0]):int(r[0]+r[2])]
       framea=cv2.cvtColor(imCrop, cv2.COLOR_YUV420p2GRAY)
       hsv=cv2.cvtColor(imCrop, cv2.COLOR_BGR2GRAY)
       blur=cv2.GaussianBlur(framea,(5,5),0)
       ret,otsu=cv2.threshold(blur,0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
       otsuf=255-otsu
       mom=cv2.moments(otsuf, binaryImage=True )
       #perc1=cv2.copyMakeBorder(otsuf,1,1,1,1, cv2.BORDER_CONSTANT)
       #can=cv2.Canny(perc1,0,254)
       cnt,hierarchy = cv2.findContours(otsuf, 1, 2)
       per=cv2.arcLength(cnt[0],True)
       Area=mom['m00']
       circ=pow(per, 2)/Area
       ecc=(pow(mom['mu20']-mom['mu02'], 2) - 4*pow(mom['mu11'] , 2))/pow(mom['mu20']+mom['mu02'],2)
       Hu=np.array(cv2.HuMoments(mom))
       area.append(Area)
       eccentricity.append(ecc)
       circularity.append(circ)
       hu.append(Hu)
   resultfinal=pd.DataFrame(list(zip(area, eccentricity, circularity, hu)), columns=['Area', 'Eccentricity', 'Circularity', 'Hu-moments'])
   resultfinal.to_csv('resultados'+str(video)+'.csv', header=True)

video=('NEGATIVE2-T3M1.mp4')


for i in filenames:
    analisisvideo(i)
analisisvideo(video)

newnegs=np.array(["SYPHON1-T3M3.mp4",
"SYPHON3-T2M2.mp4",
"SYPHON3-TAM4.mp4",
"SYPHON5-T3M3.mp4",
"SYPHON5-T5M1.mp4"
])
    
newnegs=newnegs.tolist()
analisisvideor(filenames)
    
rs=pd.DataFrame(rs)
rs['Video']=np.array(["SYPHON1-T3M3.mp4",
"SYPHON3-T2M2.mp4",
"SYPHON3-TAM4.mp4",
"SYPHON5-T3M3.mp4",
"SYPHON5-T5M1.mp4"
])
rs.to_csv('rois.csv')
rs=pd.read_csv('rois.csv')
rs=rs.drop('Unnamed: 0', axis=1)
Created on Thu Jul  2 01:20:42 2020

@author: felip
""")