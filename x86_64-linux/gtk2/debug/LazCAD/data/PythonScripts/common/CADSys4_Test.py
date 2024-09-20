import sys
import ctypes as ct
import ctypes.wintypes as wt
import os
import math
from ctypes import *
from ctypes import cdll
import ctypes.wintypes as wt

a = os.environ['path']
a =  os.getcwd() + ";" + a
os.environ['path'] = a

if (sys.platform == "win32"):
  a = os.environ['path']
  a =  os.getcwd() + ";" + a
  os.environ['path'] = a
  intrf = ct.windll.CADSys4AppIntrf
elif (sys.platform == "linux"):
  intrf = cdll.LoadLibrary('/media/mustafa/DATEN/daten/mustafa/entwicklung/lazarus/LazCAD/x86_64-linux/gtk2/debug/LazCAD/libcadsys4appintrf.so')
elif (sys.platform == "darwin"):
  pass# macOS code goes here.
else:
  print("OS not supported!")

CAD_AddLine = intrf.CAD_AddLine
CAD_AddLine.restype = ct.c_int
CAD_AddLine.argtypes = [ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_int, ct.c_int, ct.c_int, ct.c_wchar_p]
#CAD_AddLine.argtypes = [ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_int, ct.c_char_p]

CAD_AddArc = intrf.CAD_AddArc
CAD_AddArc.restype = ct.c_int
CAD_AddArc.argtypes = [ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_int, ct.c_int, ct.c_int, ct.c_wchar_p]

CAD_AddCircle = intrf.CAD_AddCircle
CAD_AddCircle.restype = ct.c_int
CAD_AddCircle.argtypes = [ct.c_float, ct.c_float, ct.c_float,ct.c_float, ct.c_int, ct.c_int, ct.c_int, ct.c_wchar_p]

CAD_Repaint = intrf.CAD_Repaint
CAD_Repaint.restype = ct.c_int
CAD_Repaint.argtypes = [ct.c_int]

CAD_ZoomExt = intrf.CAD_ZoomExt
CAD_ZoomExt.restype = ct.c_int
CAD_ZoomExt.argtypes = [ct.c_int]

CAD_DeleteAllObjects = intrf.CAD_DeleteAllObjects
CAD_DeleteAllObjects.restype = ct.c_int
CAD_DeleteAllObjects.argtypes = [ct.c_int]

#CAD_ZoomExt = intrf.ZoomExt
#CAD_ZoomExt.restype = ct.c_int
#CAD_ZoomExt.argtypes = [ct.c_int]

CAD_NewLayer = intrf.CAD_NewLayer
CAD_NewLayer.restype = ct.c_int
CAD_NewLayer.argtypes = [ct.c_wchar_p, ct.c_int, ct.c_int, ct.c_int, ct.c_int, ct.c_int, ct.c_int, ct.c_int]


def TestLayers():
   for i in range(1, 3):
     AName = 'TestLayer' + str(i)
     AActive = 1
     AVisible = 1
     APenColor = i ################
     APenWidth = 1
     APenStyle = 0  #'psSolid' ???
     ABrushColor = 255
     ABrushStyle = 0  ####'bsSolid' ??

     CAD_AddLayer(AName, AActive, AVisible, APenColor, APenWidth, APenStyle, ABrushColor, ABrushStyle);

def SineWave():
  for i in range(1, 180):
    x0 = i/10;
    y0 = math.sin(x0);
    x1 = x0 + 2
    y1 = y0 + 2
    PenColor = 255  #Red ???
    PenWidth = 1
    PenStyle = 0  #psSolid ???
    LayerName = 'TestLayer' + str(i)
    CAD_AddLine(x0, y0, x1, y1, PenColor, PenWidth, PenStyle, LayerName)
    CAD_ZoomExt(1)
    print(i)

def Parabol():
  r = 100;
  for i in range(-600, 600):
    x = i/100
    y = x*x
    CAD_AddLine(x, y, x+5, y+5, 100, '0')
    CAD_ZoomExt(1)

def Hyperbol():
  r = 100;
  for i in range(-600, 600):
    x = i/100
    y = -1*x*x
    CAD_AddLine(x, y, x+2, y+2, 100, '0')
    CAD_ZoomExt(1)

def DrawArc():
  for i in range(0, 300):
    cx = 0
    cy = 0
    r  = 50
    sa = 0
    ea = 180
    filled = 0
    direction = 0  #ccw
    colori = 255
    layer  = '0000'
    CAD_AddArc(cx+i, cy+i, r, sa, ea, filled, direction, colori, layer)
    CAD_ZoomExt(1)

def DrawCircle():
  for i in range(0, 300):
    cx = 0
    cy = 0
    r  = 50
    sa = 0
    filled = 0
    direction = 0  #ccw
    colori = 255
    layer  = '0'
    CAD_AddCircle(cx+i, cy+i, r, sa, filled, direction, colori, layer)
    CAD_ZoomExt(1)

for i in range(0, 1):
  CAD_DeleteAllObjects(1)
  #DrawLines()
  #TestLayers()
  SineWave()
  CAD_DeleteAllObjects(1)
  Parabol()
  CAD_DeleteAllObjects(1)
  DrawArc()
  CAD_DeleteAllObjects(1)
  DrawCircle()

  #CAD_DeleteAllObjects(1)
  #Hyperbol()
