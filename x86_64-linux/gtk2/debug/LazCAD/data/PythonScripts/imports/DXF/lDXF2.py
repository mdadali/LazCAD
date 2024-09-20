import ezdxf
import ctypes as ct
import ctypes.wintypes as wt
import os
import math
from ctypes import *
from ctypes import cdll
import ctypes.wintypes as wt

#OpenFileName = dxffilename.value
OpenFileName = 'D:/daten/mustafa/entwicklung/lazarus/LazCAD/x86_64-win64/win/debug/LazCAD/data/CAD006.DXF'

intrf = ct.windll.CADSys4AppIntrf
CAD_NewLayer = intrf.CAD_NewLayer
CAD_NewLayer.restype = ct.c_int
CAD_NewLayer.argtypes = [ct.c_wchar_p, ct.c_int, ct.c_int, ct.c_int, ct.c_int, ct.c_wchar_p, ct.c_int, ct.c_wchar_p]

CAD_Repaint = intrf.CAD_Repaint
CAD_Repaint.restype = ct.c_int
CAD_Repaint.argtypes = [ct.c_int]

CAD_AddLine = intrf.CAD_AddLine
CAD_AddLine.restype = ct.c_int
CAD_AddLine.argtypes = [ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_int, ct.c_int, ct.c_wchar_p, ct.c_wchar_p]
#CAD_AddLine.argtypes = [ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_int, ct.c_char_p]

CAD_AddArc = intrf.CAD_AddArc
CAD_AddArc.restype = ct.c_int
CAD_AddArc.argtypes = [ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_int, ct.c_int, ct.c_int, ct.c_int, ct.c_wchar_p, ct.c_wchar_p]

CAD_AddCircle = intrf.CAD_AddCircle
CAD_AddCircle.restype = ct.c_int
CAD_AddCircle.argtypes = [ct.c_float, ct.c_float, ct.c_float, ct.c_float, ct.c_int, ct.c_int, ct.c_int, ct.c_int, ct.c_wchar_p, ct.c_wchar_p]



DEG_TO_RAD = 0.01745329251994329577

def AddLayers(doc):
  #CAD_NewLayer('test', 1, 1, 2, 1, 'psdot', 2, 'bssolid')
  for layer in doc.layers:
    if layer.dxf.name != '0':
      print(layer.dxf.color)
      Name       = layer.dxf.name
      Active     = not layer.is_locked()
      Visible    = layer.is_on()
      PenColor   = layer.dxf.color
      PenWidth   = 1  #layer.dxf.thickness
      PenStyle   = layer.dxf.linetype  #'psSolid' ???
      BrushColor = 5  #layer.dxf.brush_color
      BrushStyle = 'bssolid'  #layer.dxf.brushtype

      CAD_NewLayer(Name, Active, Visible, PenColor, PenWidth, PenStyle, BrushColor, BrushStyle)


################################################################################
#def draw_point_entity(e):
#  #Litecad64.lcBlockAddPoint(hLcBlock, e.dxf.location.x, e.dxf.location.y)
#  pass
################################################################################
def draw_line_entity(e):
 PenWidth = 1
 PenStyle = 'psdot'
 CAD_AddLine(e.dxf.start.x, e.dxf.start.y ,e.dxf.end.x , e.dxf.end.y,  e.dxf.color, PenWidth, e.dxf.linetype, e.dxf.layer)
################################################################################
def draw_arc_entity(e):
  cp = e.dxf.center
  sa = e.dxf.start_angle
  ea = e.dxf.end_angle
  r  = e.dxf.radius
  cx = cp[0]
  cy = cp[1]

  filled    = 0
  direction = 0

  color    = e.dxf.color
  linewidth = e.dxf.thickness
  linetype = e.dxf.linetype
  penstyle = 'psdot'
  layer    = e.dxf.layer

  CAD_AddArc(cx, cy, r, sa, ea, filled, direction, color, linewidth, linetype, layer)
################################################################################
def draw_circle_entity(e):
  cp = e.dxf.center
  r  = e.dxf.radius
  cx = cp[0]
  cy = cp[1]
  sa = 0.0
  filled    = 0
  direction = 0

  color    = e.dxf.color
  linewidth = e.dxf.thickness
  linetype = e.dxf.linetype
  penstyle = 'psdot'
  layer    = e.dxf.layer

  CAD_AddArc(cx, cy, r, sa, filled, direction, color, linewidth, linetype, layer)
################################################################################
def read_dxf_file():
  doc = ezdxf.readfile(OpenFileName)
  msp = doc.modelspace()
  AddLayers(doc)
  for e in msp:
    if e.dxftype() == "LINE":
      draw_line_entity(e)
    elif e.dxftype() == "ARC":
      draw_arc_entity(e)
    elif e.dxftype() == "CIRCLE":
      draw_circle_entity(e)
  CAD_Repaint(1)
################################################################################
def main():
  read_dxf_file()

if __name__ == "__main__":
  main()



