#importar librerias##
import cv2
import numpy as np
import os 
from  matplotlib import pyplot as plt
import glob
import pandas as pd
##determinando inputs interactivamente##
directorio=input("Por favor digite el nombre de todo el directorio:")
mascaray=[input("input coordinatey one by one:") for i in range(2)]
mascarax=[input("input coordinate x one by one:") for i in range(2)]
iniciofinal=[input("Por favor digite los cuadros de inicio y final del experimento:") for i in range(2)]
#determinando directiorio##
#path =r"C:\Users\felip\Documents\PhD\Douda\experiments joanna\Archive Joannas experiment\JOanna experiment archive\test 2 100 mg\Mussel 2\Aligned"
path =r"C:\Users\felip\Documents\PhD\Douda\experiments joanna\Second approach\5fps\sifon1images"
os.chdir(path)

##creando una mascara
mask = np.zeros((240,320), np.uint8)
#mask[61:131,122:186]=255#
mask[int(mascaray[0]):int(mascaray[1]),int(mascarax[0]):int(mascarax[1])]=255
##leyendo las imagenes
filenames = glob.glob("*.png")
mussel1 = [cv2.imread(img,0) for img in filenames]
##verificar mascara con cuadros de inicio y final (respecto a la inoculación##
masked_imgI=cv2.bitwise_and(mussel1[int(iniciofinal[0])], mussel1[int(iniciofinal[0])], mask=mask)
masked_imgF=cv2.bitwise_and(mussel1[int(iniciofinal[1])], mussel1[int(iniciofinal[1])], mask=mask)
plt.figure(), plt.imshow(masked_imgI)
plt.figure(), plt.imshow(masked_imgF)
##calculando todos los histogramas###
hist=[cv2.calcHist([slide],[0],mask,[256],[0,256]) for slide in mussel1]

##comparando los histogramas##
indices0=[]
indices1=[]
indices2=[]
indices3=[]
for i in [0,1,2,3]:
    if i == 0:
        for s in range(len(hist)-1):
            d=cv2.compareHist(hist[s],hist[s+1],0)
            indices0.append(d)
    elif i == 1:
        for s in range(len(hist)-1):
            d=cv2.compareHist(hist[s],hist[s+1],1)
            indices1.append(d)
    elif i == 2:
        for s in range(len(hist)-1):
            d=cv2.compareHist(hist[s],hist[s+1],2)
            indices2.append(d)
    else:
        for s in range(len(hist)-1):
            d=cv2.compareHist(hist[s],hist[s+1],3)
            indices3.append(d)
##transformando las listas en arrays in exportando al clipboard##  
indices0array = np.array(indices0)
indices1array = np.array(indices1)
indices2array = np.array(indices2)    
indices3array = np.array(indices3)   
union = {'Correlación': indices0array, 'Chisq': indices1array, 'Intersec': indices2array, 'Bhattacharyya': indices3array}  
resultados = pd.DataFrame(data=union)
resultados.to_clipboard(sep=',')
 #resultados.to_csv(r'C:\Users\felip\Documents\PhD\Douda\experiments joanna\Archive Joannas experiment\JOanna experiment archive\test 1 50 mg ml\Mussel 3\hist.csv',header=True)
