# -*- coding: utf-8 -*-
"""Copy of Final project 5.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1HyF5hBdIymHftjtuD9MgRhYjKpg0ysYL
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Conv2D
from tensorflow.keras.layers import MaxPool2D
from tensorflow.keras.layers import Flatten
from tensorflow.keras.layers import Dense
from sklearn.utils import shuffle
import cv2
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.optimizers import Adam
from google.colab.patches import cv2_imshow

data = pd.read_csv(r"A_Z Handwritten Data.csv").astype('float32')
print(data.head())
print(data.shape)

x = data.drop('0',axis=1)
y = data['0']
x_train,x_test,y_train,y_test = train_test_split(x,y,test_size=0.2)

x_train = np.reshape(x_train.values, (x_train.shape[0], 28,28))
x_test = np.reshape(x_test.values, (x_test.shape[0], 28,28))
print("Train data shape: ", x_train.shape)
print("Test data shape: ", x_test.shape)

words = {0:'A',1:'B',2:'C',3:'D',4:'E',5:'F',6:'G',7:'H',8:'I',9:'J',10:'K',11:'L',12:'M',13:'N',14:'O',15:'P',16:'Q',17:'R',18:'S',19:'T',20:'U',21:'V',22:'W',23:'X', 24:'Y',25:'Z'}
y_int = np.int0(y)
count = np.zeros(26, dtype='int')
for i in y_int:
    count[i] +=1

alphabets = []
for i in words.values():
    alphabets.append(i)
fig, ax = plt.subplots(1,1, figsize=(10,10))

ax.barh(alphabets, count)

plt.xlabel("Number of elements ")
plt.ylabel("Alphabets")
plt.grid()
plt.show()

shuff = shuffle(x_train[:100])

fig, ax = plt.subplots(3,3, figsize = (10,10))
axes = ax.flatten()

for i in range(9):
    _, shu = cv2.threshold(shuff[i], 30, 200, cv2.THRESH_BINARY)
    axes[i].imshow(np.reshape(shuff[i], (28,28)), cmap="Greys")
plt.show()

x_train = x_train.reshape(x_train.shape[0],x_train.shape[1],x_train.shape[2],1)
x_test = x_test.reshape(x_test.shape[0],x_train.shape[1],x_test.shape[2],1)
y_new = to_categorical(y_train,num_classes=26,dtype='int')
y_new_test = to_categorical(y_test,num_classes=26,dtype='int')
print("New Shape of Train data is: ",x_train.shape)
print("New Shape of Test data is: ",x_test.shape)
print("New Shape of Label Train is: ",y_new.shape)
print("New Shape of Label Test is: ",y_new_test.shape)

model = Sequential()
model.add(Conv2D(filters=32,kernel_size=(3,3),activation='relu',input_shape=(28,28,1)))
model.add(MaxPool2D(pool_size=(2,2),strides=2))
model.add(Conv2D(filters=64,kernel_size=(3,3),activation='relu',padding='same'))
model.add(MaxPool2D(pool_size=(2,2),strides=2))
model.add(Conv2D(filters=128,kernel_size=(3,3),activation='relu',padding='valid'))
model.add(MaxPool2D(pool_size=(2,2),strides=2))
model.add(Flatten())
model.add(Dense(64,activation='relu'))
model.add(Dense(128,activation='relu'))
model.add(Dense(26,activation='softmax'))
model.compile(optimizer=Adam(learning_rate =.001),loss='categorical_crossentropy',metrics=['accuracy'])
tory = model.fit(x_train,y_new,epochs=1,validation_data=(x_test,y_new_test))

fig, axes = plt.subplots(3,3, figsize=(8,9))
axes = axes.flatten()

for i,ax in enumerate(axes):
    img = np.reshape(x_test[i], (28,28))
    ax.imshow(img, cmap="Greys")
    
    pred = words[np.argmax(y_new_test[i])]
    ax.set_title("Prediction: "+pred)
    ax.grid()

print(model.summary())

img = cv2.imread(r'Alphabet 2.jpg')
img_copy = img.copy()

img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
img = cv2.resize(img, (400,440))

img_copy = cv2.GaussianBlur(img_copy, (7,7), 0)
img_gray = cv2.cvtColor(img_copy, cv2.COLOR_BGR2GRAY)
_, img_thresh = cv2.threshold(img_gray, 100, 255, cv2.THRESH_BINARY_INV)

img_final = cv2.resize(img_thresh, (28,28))
img_final =np.reshape(img_final, (1,28,28,1))

img_pred = words[np.argmax(model.predict(img_final))]

cv2.putText(img, "Sohila _ _ _ ", (20,25), cv2.FONT_HERSHEY_TRIPLEX, 0.7, color = (0,0,230))
cv2.putText(img, "Prediction: " + img_pred, (20,410), cv2.FONT_HERSHEY_DUPLEX, 1.3, color = (255,0,30))
cv2_imshow( img)

model.save('Sohila 2 project')