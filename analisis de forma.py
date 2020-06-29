# -*- coding: utf-8 -*-
"""
Created on Mon May 25 16:47:31 2020

@author: felip
"""
import cv2
import numpy as np
import os 
from  matplotlib import pyplot as plt
import glob
#import pandas as pd
##seleccionando directiorio en este momento no es interactivo
path =r"C:\Users\felip\Documents\PhD\Douda\experiments joanna\Second approach\5fps\sifon1images"
os.chdir(path)
##leyendo las imagenes no use imagej para dividir los videos use ffmpeg
filenames = glob.glob("*.png")
mussel1 = [cv2.imread(img,0) for img in filenames]
##creando  gaussian blur
blur=[]
for s in range (len(mussel1)):
    resu=cv2.GaussianBlur(mussel1[s],(5,5),0)
    blur.append(resu)
##calculando umbral con algoritmo de otsu
otsu=[]
for s in range(len(blur)):
    ret,resu=cv2.threshold(blur[s],0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
    resuf=255-resu
    otsu.append(resuf)
##calculando  y dibujando contornos
contornos=[]
for s  in range (len(otsu)):
    cnt, hrq =cv2.findContours(otsu[s], cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    contornos.append(cnt[0])
for s in range(len(mussel1)):
    cv2.drawContours(mussel1[s], contornos[s], -1, (0, 0, 0),  3  )
   #  active esto cuando quiera exportar name='contorno'+str(s)+'.jpg'
    #cv2.imwrite(name,mussel1[s])
    
#comparar los contornos usando descriptores de forma, momentos de Hull y momentos de Zernicke 
## ademas de otros descriptores normales
#momentos de Hu
Hum=[]
for s in range(len(contornos)):
    res=cv2.matchShapes(contornos[s], contornos[s+1], cv2.CONTOURS_MATCH_I3, 0.0)
    Hum.append(res)

##probemos con el area y el perimtetro
areasyperimetros=np.empty(shape=(len(contornos),3), dtype='object')
for s in range(len(contornos)):
    res=cv2.contourArea(contornos[s])
    perc=cv2.arcLength(contornos[s], True)
    pera=cv2.arcLength(contornos[s], False)
    areasyperimetros[s,0]=res
    areasyperimetros[s,1]=perc
    areasyperimetros[s,2]=pera

rango=range(1,len(mussel1),1)
for s in range(len(mussel1)):
    name='video'+'00'+str(s)+'.jpg'
    plt.subplot(131)
    plt.imshow(mussel1[s+1])
    plt.subplot(232)
    plt.plot(Hum)
    plt.plot(Hum, 'ro')
    plt.axvline(x=s)
    plt.title('Hum moments comparison')
    plt.subplot(233)
    plt.plot(areasyperimetros[rango,0])
    plt.plot(areasyperimetros[rango,0], 'ro')
    plt.axvline(x=s)
    plt.title('Area')
    plt.subplot(235)
    plt.plot(areasyperimetros[rango,1])
    plt.plot(areasyperimetros[rango,1], 'ro')
    plt.axvline(x=s)
    plt.title('Perimetro cerrado')
    plt.subplot(236)
    plt.plot(areasyperimetros[rango,2])
    plt.plot(areasyperimetros[rango,2], 'ro')
    plt.axvline(x=s)
    plt.title('Perimetro abierto')
    plt.savefig(name)
    plt.close()



    


#cv2.drawContours(mussel1[1], contornos, 0, (0, 0, 0),  0  )
#plt.figure()
#plt.imshow(mussel1[1], 'gray')
#plt.imshow(blur[1])